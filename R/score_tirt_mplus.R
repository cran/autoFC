#' Fast Analytical Trait Scoring for Mplus TIRT/TFM Models
#'
#' @description Extracts the estimated parameters from an Mplus .out file and 
#' uses a fast analytical gradient in R to instantly calculate MAP trait scores 
#' and Standard Errors. Works seamlessly for both First-Order (TIRT) and Second-Order (TFM) models.
#'
#' @param inp_file Character. Path to the Mplus .inp (or .out) file.
#' @param data The pairwise binary dataset used to fit the model.
#' @param trait_names Optional character vector. The original names of the traits to 
#'        use as column names in the final output. If NULL, uses the Mplus generated names.
#' @param run_model Logical. If TRUE, runs Mplus before scoring. If FALSE, assumes 
#'        the .out file already exists. Defaults to TRUE.
#'
#' @return A data frame containing Trait scores and their Standard Errors (SE).
#' @export
score_tirt_mplus <- function(inp_file, data, trait_names = NULL, run_model = TRUE) {
  
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    stop("The 'MplusAutomation' package is required. Please install it.")
  }
  
  has_pb <- requireNamespace("pbapply", quietly = TRUE)
  
  # ------------------------------------------------------------------
  # 1. Run the Model & Find the Output File
  # ------------------------------------------------------------------
  if (run_model) {
    cat(sprintf("Running Mplus model: %s\n", inp_file))
    MplusAutomation::runModels(inp_file, showOutput = TRUE)
  }
  
  out_file <- sub("\\.inp$", ".out", inp_file)
  if (!file.exists(out_file)) out_file <- sub("\\.inp$", ".out", tolower(inp_file))
  if (!file.exists(out_file)) stop(sprintf("Could not find the Mplus output file (%s). Check for syntax errors.", out_file))
  
  # ------------------------------------------------------------------
  # 2. Read Parameters & Auto-Detect Traits via paramHeader Grouping
  # ------------------------------------------------------------------
  cat("Extracting parameters from Mplus output...\n")
  res <- MplusAutomation::readModels(out_file)
  params <- res$parameters$unstandardized
  
  if (is.null(params)) stop("No parameters found in .out file. Model may not have converged.")
  
  pair_names <- toupper(colnames(data))
  n_pairs <- length(pair_names)
  
  # Group by .BY headers to find latent variables
  all_by_headers <- unique(params$paramHeader[grepl("\\.BY$", params$paramHeader)])
  # Group by .WITH headers to find correlations for traits and (in the case of TIRT) for correlated residuals
  all_with_headers <- unique(params$paramHeader[grepl("\\.WITH$", params$paramHeader)])
  
  # Utilities always start with T and a number (e.g., T1.BY, T20.BY)
  util_headers <- grep("^T\\d+\\.BY$", all_by_headers, value = TRUE)
  is_tfm <- length(util_headers) > 0
  
  # For TIRT, we have correlated residuals between pairs
  pair_with_headers <- unique(params$paramHeader[grepl("I\\d+I\\d+\\.WITH$", params$paramHeader)])
  trait_with_headers <- setdiff(all_with_headers, pair_with_headers)
  
  # The remaining .BY headers are the Traits!
  trait_headers <- setdiff(all_by_headers, util_headers)
  D <- length(trait_headers)
  
  if (D == 0) stop("Could not detect any trait headers in the Mplus output.")
  
  # Match user-provided names, or default to the Mplus names
  if (is.null(trait_names)) {
    trait_names <- sub("\\.BY$", "", trait_headers)
  } else if (length(trait_names) != D) {
    stop(sprintf("You provided %d trait_names, but %d traits were detected in the model.", length(trait_names), D))
  }
  
  # Initialize Matrices
  L_marginal <- matrix(0, nrow = n_pairs, ncol = D)
  tau <- rep(0, n_pairs)
  V_res <- rep(0, n_pairs)
  Omega <- diag(D)
  
  
  # ------------------------------------------------------------------
  # 3. Extract Loadings (L_marginal) using Structural Indices
  # ------------------------------------------------------------------
  for (t_idx in 1:D) {
    t_loads <- params[params$paramHeader == trait_headers[t_idx], ]
    
    if (is_tfm) {
      # TFM: Traits load on Utilities, Utilities load on Pairs
      for (i in 1:nrow(t_loads)) {
        util_name <- t_loads$param[i]
        t_val <- t_loads$est[i]
        
        u_pairs <- params[params$paramHeader == paste0(util_name, ".BY"), ]
        for (j in 1:nrow(u_pairs)) {
          p_idx <- match(u_pairs$param[j], pair_names)
          if (!is.na(p_idx)) L_marginal[p_idx, t_idx] <- L_marginal[p_idx, t_idx] + (t_val * u_pairs$est[j])
        }
      }
    } else {
      # TIRT: Traits load directly on Pairs
      for (i in 1:nrow(t_loads)) {
        p_idx <- match(t_loads$param[i], pair_names)
        if (!is.na(p_idx)) L_marginal[p_idx, t_idx] <- t_loads$est[i]
      }
    }
  }
  
  # ------------------------------------------------------------------
  # 4. Extract Residual Variances and Thresholds
  # ------------------------------------------------------------------
  res_vars <- params[params$paramHeader == "Residual.Variances", ]
  thresholds <- params[params$paramHeader == "Thresholds", ]
  
  for (p_idx in 1:n_pairs) {
    p_name <- pair_names[p_idx]
    
    # Thresholds
    t_val <- thresholds$est[thresholds$param == p_name]
    if (length(t_val) > 0) tau[p_idx] <- t_val[1]
    
    # Residual Variances
    if (is_tfm) {
      pair_res <- res_vars$est[res_vars$param == p_name]
      if (length(pair_res) == 0) pair_res <- 0
      
      # Which utilities load onto this pair?
      u_headers_for_pair <- params$paramHeader[grepl("^T\\d+\\.BY$", params$paramHeader) & params$param == p_name]
      u_names <- sub("\\.BY$", "", u_headers_for_pair)
      util_res_sum <- sum(res_vars$est[res_vars$param %in% u_names])
      
      V_res[p_idx] <- pair_res[1] + util_res_sum
    } else {
      p_res <- res_vars$est[res_vars$param == p_name]
      if (length(p_res) > 0) V_res[p_idx] <- p_res[1]
    }
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
  # 5. Extract Covariances (Omega)
  # ------------------------------------------------------------------
  cors <- params[params$paramHeader %in% trait_with_headers, ]
  header_lvs <- unique(cors$paramHeader)
  param_lvs <- unique(cors$param)
  for (i in 1:length(header_lvs)) {
     for (j in (i):length(param_lvs)) {
        val <- cors$est[cors$paramHeader == header_lvs[i] & cors$param == param_lvs[j]] 
        Omega[i, j+1] <- Omega[j+1, i] <- val[1]
     }
  }
  Omega_inv <- solve(Omega)
  # print(Omega)
  # ------------------------------------------------------------------
  # 5. Analytical Optimization Loop (BFGS)
  # ------------------------------------------------------------------
  cat(sprintf("\nScoring %s respondents using BFGS (%s parameterization)...\n", 
              nrow(data), ifelse(is_tfm, "TFM", "TIRT")))
  
  
  data_mat <- as.matrix(data)
  apply_func <- if (has_pb) pbapply::pbapply else apply
  
  results <- apply_func(data_mat, 1, function(y_vec) {
    valid <- !is.na(y_vec)
    if (sum(valid) == 0) return(rep(NA, D * 2))
    
    y_v <- y_vec[valid]; L_v <- L_marginal[valid, , drop = FALSE]
    tau_v <- tau[valid]; sd_v <- sd_res[valid]
    q <- ifelse(y_v == 1, 1, -1)
    
    neg_log_post <- function(eta) {
      mu_star <- (L_v %*% eta - tau_v) / sd_v
      z <- q * mu_star
      return(-(sum(pnorm(z, log.p = TRUE)) - 0.5 * as.numeric(t(eta) %*% Omega_inv %*% eta)))
    }
    
    gradient <- function(eta) {
      mu_star <- (L_v %*% eta - tau_v) / sd_v
      z <- q * mu_star
      mills <- exp(dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE))
      return(-(t(L_v / sd_v) %*% (q * mills) - (Omega_inv %*% eta)))
    }
    
    opt <- tryCatch({
      optim(par = rep(0, D), fn = neg_log_post, gr = gradient, method = "BFGS", hessian = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(opt) || opt$convergence != 0) return(rep(NA, D * 2))
    
    scores <- opt$par
    ses <- tryCatch({ sqrt(diag(solve(opt$hessian))) }, error = function(e) rep(NA, D))
    return(c(scores, ses))
  })
  
  # ------------------------------------------------------------------
  # 6. Format Output
  # ------------------------------------------------------------------
  res_df <- as.data.frame(t(results))
  colnames(res_df) <- c(trait_names, paste0(trait_names, "_SE"))
  
  return(res_df)
}
