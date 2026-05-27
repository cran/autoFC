#' Predict Trait Scores for New Data using a Fitted Stan Model
#'
#' @description Extracts posterior means of item parameters from a fitted Stan TIRT
#' model and uses analytical BFGS optimization to instantly score new respondents.
#'
#' @param fit A fitted model object from either cmdstanr or rstan.
#' @param stan_data The original data list passed to Stan (used to map the test design).
#' @param new_pairwise_data A dataframe of pairwise binary responses for the NEW respondents.
#'
#' @return A data frame containing MAP trait scores and Standard Errors.
#' @export
predict_tirt_stan <- function(fit, stan_data, new_pairwise_data) {

  has_pb <- requireNamespace("pbapply", quietly = TRUE)

  # 1. Extract test design and trait names from the original stan_data
  trait_names <- attr(stan_data, "trait_names")
  D <- stan_data$D
  P <- stan_data$P

  if (ncol(new_pairwise_data) != P) {
    stop("new_pairwise_data must have exactly ", P, " columns (pairs).")
  }

  cat("Extracting posterior item parameters from the fitted Stan model...\n")

  # 2. Extract Posterior Means (Robust to both cmdstanr and rstan)
  if (inherits(fit, "CmdStanMCMC")) {
    alpha  <- fit$summary("alpha", "mean")$mean
    lambda <- fit$summary("lambda", "mean")$mean
    denom  <- fit$summary("denom", "mean")$mean
    Omega_vec <- fit$summary("Omega", "mean")$mean
  } else if (inherits(fit, "stanfit")) {
    alpha  <- rstan::summary(fit, pars = "alpha")$summary[, "mean"]
    lambda <- rstan::summary(fit, pars = "lambda")$summary[, "mean"]
    denom  <- rstan::summary(fit, pars = "denom")$summary[, "mean"]
    Omega_vec <- rstan::summary(fit, pars = "Omega")$summary[, "mean"]
  } else {
    stop("The 'fit' object must be a fitted model from cmdstanr or rstan.")
  }

  # Format Trait Covariance Matrix (Omega)
  Omega <- matrix(Omega_vec, nrow = D, ncol = D)
  Omega_inv <- solve(Omega)

  # 3. Collapse the Math: Build the Marginal Loadings Matrix
  # This analytically marginalizes the utilities out of the equation
  L_marginal <- matrix(0, nrow = P, ncol = D)

  for (p in 1:P) {
    i <- stan_data$item_i[p]
    k <- stan_data$item_k[p]
    t_i <- stan_data$trait[i]
    t_k <- stan_data$trait[k]

    # Loadings of traits on the pair outcome
    L_marginal[p, t_i] <- L_marginal[p, t_i] + lambda[i]
    L_marginal[p, t_k] <- L_marginal[p, t_k] - lambda[k]
  }

  # Scale by the denominator (sqrt of sum of uniquenesses)
  L_marginal <- sweep(L_marginal, 1, denom, "/")
  int_marginal <- alpha / denom

  # 4. Prepare for Optimization
  data_mat <- as.matrix(new_pairwise_data)
  cat(sprintf("Scoring %s NEW respondents using BFGS...\n", nrow(data_mat)))

  apply_func <- if (has_pb) pbapply::pbapply else apply

  # 5. Optimization Loop over Respondents
  results <- apply_func(data_mat, 1, function(y_vec) {

    # Handle missing/MOLE data cleanly
    valid <- !is.na(y_vec)
    if (sum(valid) == 0) return(rep(NA, D * 2))

    y_v <- y_vec[valid]
    L_v <- L_marginal[valid, , drop = FALSE]
    int_v <- int_marginal[valid]

    # Map 0/1 to -1/1 for the Probit link
    q <- ifelse(y_v == 1, 1, -1)

    # -- Objective Function (Negative Log-Posterior) --
    neg_log_post <- function(eta) {
      mu_star <- as.numeric(L_v %*% eta + int_v)
      z <- q * mu_star

      ll <- sum(pnorm(z, log.p = TRUE))
      lprior <- -0.5 * as.numeric(t(eta) %*% Omega_inv %*% eta)

      return(-(ll + lprior))
    }

    # -- Analytical Gradient --
    gradient <- function(eta) {
      mu_star <- as.numeric(L_v %*% eta + int_v)
      z <- q * mu_star

      mills <- exp(dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE))

      grad_ll <- t(L_v) %*% (q * mills)
      grad_prior <- -(Omega_inv %*% eta)

      return(-(grad_ll + grad_prior))
    }

    # -- Execute BFGS Optimization --
    init_eta <- rep(0, D)
    opt <- tryCatch({
      optim(par = init_eta, fn = neg_log_post, gr = gradient,
            method = "BFGS", hessian = TRUE)
    }, error = function(e) NULL)

    if (is.null(opt) || opt$convergence != 0) return(rep(NA, D * 2))

    # Extract Scores
    scores <- opt$par

    # Extract Standard Errors from the inverted Hessian matrix
    ses <- tryCatch({
      sqrt(diag(solve(opt$hessian)))
    }, error = function(e) rep(NA, D))

    return(c(scores, ses))
  })

  # 6. Format the Output
  res_df <- as.data.frame(t(results))
  colnames(res_df) <- c(trait_names, paste0(trait_names, "_SE"))

  return(res_df)
}
