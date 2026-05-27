#' Calculate Overall or Binned RMSE of Trait Scores
#'
#' @description A diagnostic function to examine measurement accuracy. It calculates 
#' the Root Mean Square Error (RMSE) either overall, or within specific intervals 
#' on the latent trait continuum.
#'
#' @param true_scores Numeric vector. Actual/known trait scores.
#' @param estimated_scores Numeric vector. Estimated trait scores from the model.
#' @param range_breaks Numeric vector. Optional. Specifies the cut points for the trait 
#'        score bins (e.g., \code{c(-3, -1, 1, 3)}). If \code{NULL}, calculates overall RMSE.
#' 
#' @returns If \code{range_breaks} is \code{NULL}, returns a single numeric RMSE value.
#'          If \code{range_breaks} is specified, returns a named numeric vector showing 
#'          the RMSE within each score bin.
#' 
#' @examples
#' true <- rnorm(100)
#' est <- true + rnorm(100, 0, 0.3)
#' 
#' # Overall RMSE
#' RMSE_range(true, est)
#' 
#' # Binned RMSE
#' RMSE_range(true, est, range_breaks = c(-3, -1, 1, 3))
#'
#' @export
RMSE_range <- function(true_scores, estimated_scores, range_breaks = NULL) {
  
  # Helper: Fast native calculation of RMSE
  calc_rmse <- function(sq_err) sqrt(mean(sq_err, na.rm = TRUE))
  
  # Pre-calculate squared errors once (Vectorized math)
  sq_errors <- (true_scores - estimated_scores)^2
  
  # 1. Overall RMSE (If no breaks provided)
  if (is.null(range_breaks) || length(range_breaks) <= 1 || any(is.na(range_breaks))) {
    return(calc_rmse(sq_errors))
  }
  
  # 2. Binned RMSE
  # include.lowest = TRUE prevents the lowest score from being assigned NA
  score_cut <- cut(true_scores, breaks = range_breaks, include.lowest = TRUE)
  
  # Vectorized grouping using tapply (Exponentially faster than a for-loop)
  rmse_ranges <- tapply(X = sq_errors, INDEX = score_cut, FUN = calc_rmse)
  
  # Convert missing bins (NaN) to NA for cleaner output
  rmse_ranges[is.nan(rmse_ranges)] <- NA_real_
  
  return(rmse_ranges)
}
