#' Generate Simulated Person and Item Parameter Matrices for the TIRT Model
#'
#' @description Takes the factor analysis results extracted by \code{get_CFA_estimates()} 
#' and generates simulated person traits, item parameters, and latent utility values 
#' for a Thurstonian IRT model.
#'
#' @param cfa_estimates A list object returned by \code{get_CFA_estimates()}.
#' @param N Integer. Number of simulated responses (participants) to generate.
#' @param empirical Logical. As in \code{MASS::mvrnorm()}: Should mu and sigma 
#'        specify the empirical, rather than population, mean and covariance? 
#'        Defaults to FALSE.
#'
#' @returns A list containing matrices for \code{Lambda} (Loadings), \code{Mu} (Intercepts), 
#'          \code{Epsilon} (Residuals), \code{Theta} (Latent Traits), and \code{Utility}.
#' @example man/examples/Simulation.R
#' @importFrom MASS mvrnorm
#' @export
get_simulation_matrices <- function(cfa_estimates, N, empirical = FALSE) {
  
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("The 'MASS' package is required.")
  }
  
  # 1. Auto-extract components to prevent User Argument Overload
  loadings <- cfa_estimates$loadings
  intercepts <- cfa_estimates$intercepts
  residuals <- cfa_estimates$residuals
  covariances <- cfa_estimates$covariances
  
  # 2. Auto-detect dimensions safely
  dim_names <- rownames(covariances)
  N_dims <- length(dim_names)
  
  item_names <- intercepts$lhs
  N_items <- length(item_names)
  
  # 3. Safely Build Factor Loading Matrix (Lambda)
  Lambda <- matrix(0, nrow = N_items, ncol = N_dims, dimnames = list(item_names, dim_names))
  
  # Map loadings using exact indexing to prevent sorting mismatches
  for (i in 1:nrow(loadings)) {
    t_idx <- match(loadings$lhs[i], dim_names)
    i_idx <- match(loadings$rhs[i], item_names)
    if (!is.na(t_idx) && !is.na(i_idx)) {
      Lambda[i_idx, t_idx] <- loadings$est[i]
    }
  }
  
  # 4. Safely Build Intercepts and Residuals (Ensuring exact item order)
  mu_vec <- intercepts$est[match(item_names, intercepts$lhs)]
  Mu <- matrix(rep(mu_vec, N), nrow = N, ncol = N_items, byrow = TRUE)
  
  res_vec <- residuals$est[match(item_names, residuals$lhs)]
  
  # 5. Simulate Traits and Residuals
  Epsilon <- MASS::mvrnorm(N, mu = rep(0, N_items), Sigma = diag(res_vec), empirical = empirical)
  Theta <- MASS::mvrnorm(N, mu = rep(0, N_dims), Sigma = covariances, empirical = empirical)
  
  # 6. Calculate Overall Utility (Y* = Mu + Lambda*Theta + Epsilon)
  Utility <- Mu + (Theta %*% t(Lambda)) + Epsilon
  
  colnames(Utility) <- item_names
  
  return(list(
    Lambda = Lambda, 
    Mu = Mu,
    Epsilon = Epsilon, 
    Theta = Theta,
    Utility = as.data.frame(Utility)
  ))
}






