#' Fast, Analytical Trait Scoring for Second-Order TIRT using optim()
#'
#' @description Calculates MAP trait scores and Standard Errors for a fitted 
#' lavaan model. Automatically detects whether the model uses the First-Order 
#' (TIRT) or Second-Order (TFM) parameterization.
#'
#' @param fit A fitted lavaan object.
#' @param data The pairwise binary dataset used to fit the model.
#' @param trait_names Character vector of the traits to score (e.g., c("Trait1", "Trait2")). 
#'        Can be left NULL and detected by the function.
#' @param floor_value Numeric. If negative residual variances for observed variables are provided,
#'        replace those negative residual variances with this floor value.
#'
#' @return A data frame containing Trait scores and their Standard Errors (SE).
#' @export
score_tirt_lavaan <- function(fit, data, trait_names = NULL, floor_value = 0.0001) {
  
  has_pb <- requireNamespace("pbapply", quietly = TRUE)
  
  # ------------------------------------------------------------------
  # 1. Extract Matrices and Auto-Detect Traits
  # ------------------------------------------------------------------
  est <- lavaan::lavInspect(fit, "est")
  
  y_names <- lavaan::lavNames(fit, "ov")
  all_lv <- lavaan::lavNames(fit, "lv")
  
  # Utilities are strictly named t1, t2, ..., tN. 
  # Everything else in the latent space is a Trait!
  utils_names <- grep("^t[0-9]+$", all_lv, value = TRUE)
  is_second_order <- length(utils_names) > 0
  
  # Auto-detect traits if the user didn't provide them
  if (is.null(trait_names)) {
    trait_names <- setdiff(all_lv, utils_names)
    if (length(trait_names) == 0) {
      stop("Could not automatically detect trait names. Please provide the 'trait_names' argument.")
    }
  } else {
    # Validate user-provided traits
    if (!all(trait_names %in% all_lv)) {
      stop("Some provided 'trait_names' were not found in the lavaan model.")
    }
  }
  
  # ------------------------------------------------------------------
  # 2. Extract Marginal Loadings and Residual Variances
  # ------------------------------------------------------------------
  if (is_second_order) {
    # --- TFM (Second-Order) Logic ---
    if (!"beta" %in% names(est)) stop("Structural 'beta' matrix missing in Second-Order model.")
    
    L_yt <- est$lambda[y_names, utils_names, drop = FALSE]
    L_teta <- est$beta[utils_names, trait_names, drop = FALSE]
    
    # Collapse loadings
    L_marginal <- L_yt %*% L_teta 
    
    # Collapse residual variances
    Psi_t <- est$psi[utils_names, utils_names, drop = FALSE]
    Theta_y <- est$theta[y_names, y_names, drop = FALSE]
    V_res <- diag(L_yt %*% Psi_t %*% t(L_yt) + Theta_y)
    
  } else {
    # --- TIRT (First-Order) Logic ---
    # Traits load directly onto the pairs
    L_marginal <- est$lambda[y_names, trait_names, drop = FALSE]
    
    # Residual variances of pairs are directly in the Theta matrix
    V_res <- diag(est$theta[y_names, y_names, drop = FALSE])
  }
  if (length(V_res[V_res <= 0]) > 0) {
     cat("Residual variances for ")
  }
  n_heywood <- sum(V_res <= 0)
  if (n_heywood > 0) {
    message(sprintf(
      "Detected %d negative or zero residual variance(s) (Heywood cases). 
      \nThese were floored to 0.0001 to prevent math errors during score calculation.\n", 
      n_heywood
    ))
  }
  V_res[V_res <= 0] <- 0.0001 
  sd_res <- sqrt(V_res)
  
  # ------------------------------------------------------------------
  # 3. Extract Thresholds (tau) and Prior (Omega)
  # ------------------------------------------------------------------
  # Handle variations in how lavaan names thresholds (e.g., "|t1" vs "|th1")
  if (all(paste0(y_names, "|t1") %in% rownames(est$tau))) {
    tau <- est$tau[paste0(y_names, "|t1"), 1]
  } else if (all(paste0(y_names, "|th1") %in% rownames(est$tau))) {
    tau <- est$tau[paste0(y_names, "|th1"), 1]
  } else {
    # Fallback to pure match
    tau_names <- paste0(y_names, "|t1")
    tau <- est$tau[match(tau_names, rownames(est$tau)), 1]
  }
  
  # Extract Trait Covariance Matrix for the Bayesian Prior
  Omega <- est$psi[trait_names, trait_names, drop = FALSE]
  Omega_inv <- solve(Omega)
  
  # Prepare the data matrix
  data_mat <- as.matrix(data[, y_names, drop = FALSE])
  D <- length(trait_names)
  
  cat(sprintf("\nScoring %s respondents using BFGS (%s parameterization)...\n", 
              nrow(data_mat), ifelse(is_second_order, "Second-Order", "First-Order")))
  
  # ------------------------------------------------------------------
  # 4. Optimization Loop over Respondents
  # ------------------------------------------------------------------
  apply_func <- if (has_pb) pbapply::pbapply else apply
  
  results <- apply_func(data_mat, 1, function(y_vec) {
    
    # Handle missing data cleanly (MOLE format)
    valid <- !is.na(y_vec)
    if (sum(valid) == 0) return(rep(NA, D * 2))
    
    y_v <- y_vec[valid]
    L_v <- L_marginal[valid, , drop = FALSE]
    tau_v <- tau[valid]
    sd_v <- sd_res[valid]
    
    # Map 0/1 to -1/1 for Probit link
    q <- ifelse(y_v == 1, 1, -1)
    
    # -- Objective Function --
    neg_log_post <- function(eta) {
      mu_star <- (L_v %*% eta - tau_v) / sd_v
      z <- q * mu_star
      
      ll <- sum(pnorm(z, log.p = TRUE))
      lprior <- -0.5 * as.numeric(t(eta) %*% Omega_inv %*% eta)
      
      return(-(ll + lprior))
    }
    
    # -- Analytical Gradient --
    gradient <- function(eta) {
      mu_star <- (L_v %*% eta - tau_v) / sd_v
      z <- q * mu_star
      
      mills <- exp(dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE))
      grad_ll <- t(L_v / sd_v) %*% (q * mills)
      grad_prior <- -(Omega_inv %*% eta)
      
      return(-(grad_ll + grad_prior))
    }
    
    # Optimize
    init_eta <- rep(0, D) 
    
    opt <- tryCatch({
      optim(par = init_eta, fn = neg_log_post, gr = gradient, method = "BFGS", hessian = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(opt) || opt$convergence != 0) return(rep(NA, D * 2))
    
    scores <- opt$par
    ses <- tryCatch({
      sqrt(diag(solve(opt$hessian)))
    }, error = function(e) rep(NA, D))
    
    return(c(scores, ses))
  })
  
  # ------------------------------------------------------------------
  # 5. Format Output
  # ------------------------------------------------------------------
  res_df <- as.data.frame(t(results))
  colnames(res_df) <- c(trait_names, paste0(trait_names, "_SE"))
  
  return(res_df)
}
