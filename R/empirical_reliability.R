#' Empirical Reliability Estimates
#' @description Calculates empirical reliability for IRT-based trait scores 
#' using the formula provided by Brown & Maydeu-Olivares (2018).
#'
#' @param dataset A data frame containing the trait estimates and standard errors.
#' @param score_names Character vector. The names of the columns specifying trait scores.
#' @param se_names Optional character vector. The names of the columns specifying trait 
#'        standard errors. If \code{NULL}, the function automatically searches for columns 
#'        named \code{"[score_name]_SE"}.
#' 
#' @details For trait scores estimated using item response theory (IRT) models, a single 
#' test-level reliability coefficient (like Cronbach's Alpha) is often inappropriate 
#' because the standard error of measurement varies across the latent continuum. 
#' 
#' Empirical reliability provides a summary estimate of how reliable the trait scores 
#' are "as a whole" by comparing the variance of the estimated scores to the average 
#' error variance.
#'
#' @returns A named numeric vector containing the empirical reliability estimates 
#'          for each trait.
#'
#' @author Mengtong Li
#' @importFrom stats var
#' 
#' @examples 
#' # Create fake scores and standard errors
#' fake_scores <- data.frame(
#'   Trait1 = rnorm(100), Trait1_SE = runif(100, 0.1, 0.3),
#'   Trait2 = rnorm(100), Trait2_SE = runif(100, 0.2, 0.4)
#' )
#' 
#' # Auto-detects the "_SE" columns
#' empirical_reliability(fake_scores, score_names = c("Trait1", "Trait2"))
#' 
#' @references 
#' Brown, A., & Maydeu-Olivares, A. (2018). Ordinal factor analysis of graded-preference 
#' questionnaire data. \emph{Structural Equation Modeling, 25}(4), 516-529. \doi{10.1080/10705511.2017.1392247}
#' 
#' @export
empirical_reliability <- function(dataset, score_names, se_names = NULL) {
  
  if (!is.data.frame(dataset)) stop("'dataset' must be a data frame.")
  if (!all(score_names %in% colnames(dataset))) stop("Not all 'score_names' found in dataset.")
  
  # Auto-detect SE names if not provided
  if (is.null(se_names)) {
    se_names <- paste0(score_names, "_SE")
    if (!all(se_names %in% colnames(dataset))) {
      stop("Could not auto-detect '_SE' columns. Please provide 'se_names' manually.")
    }
  } else {
    if (length(score_names) != length(se_names)) {
      stop("'score_names' and 'se_names' must be the same length.")
    }
    if (!all(se_names %in% colnames(dataset))) stop("Not all 'se_names' found in dataset.")
  }
  
  # Vectorized calculation (Fast and clean)
  n <- nrow(dataset)
  
  rel_estimates <- sapply(seq_along(score_names), function(i) {
    score_col <- dataset[[ score_names[i] ]]
    se_col    <- dataset[[ se_names[i] ]]
    
    # Formula: Var(theta) / [Var(theta) + Mean(SE^2)]
    # (Using mean(se^2) is mathematically identical to sum(se^2)/n, but cleaner)
    score_var <- var(score_col, na.rm = TRUE)
    mean_err_var <- mean(se_col^2, na.rm = TRUE)
    
    return(score_var / (score_var + mean_err_var))
  })
  
  # Assign trait names to the output vector for clear reading
  names(rel_estimates) <- score_names
  
  return(rel_estimates)
}