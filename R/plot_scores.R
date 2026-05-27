#' Scatter Plot for True vs Estimated Scores, True Score vs Absolute Error, etc.
#'
#' @description This function provides a simple, enhanced diagnostic plot examining
#' the performance of the forced-choice scale based on simulated or known true scores.
#'
#' @param x_scores Numeric vector. Scores to be plotted on the x-axis (typically True Scores).
#' @param y_scores Numeric vector. Scores to be plotted on the y-axis (typically Estimated Scores).
#' @param type Character. Which type of plot to display? Options are \code{"simple"} 
#'        for an x-y scatter plot, or \code{"abs.diff"} for plotting the absolute 
#'        difference \code{abs(y - x)} against x. Defaults to \code{"simple"}.
#' @param ... Additional graphical parameters passed to \code{plot()} (e.g., \code{main}, \code{pch}).
#' 
#' @details This function extends base R \code{plot()} by automatically adding 
#' standard diagnostic reference lines (a 1:1 identity line for "simple", and a 
#' 0-error baseline for "abs.diff") to aid visual inspection of bias.
#'
#' @returns A base R scatter plot.
#'
#' @examples
#' true <- rnorm(100)
#' est <- true + rnorm(100, 0, 0.3)
#' plot_scores(true, est, type = "simple", main = "Recovery")
#' plot_scores(true, est, type = "abs.diff", main = "Error Magnitude")
#'
#' @export
plot_scores <- function(x_scores, y_scores, type = c("simple", "abs.diff"), ...) {
  
  # 1. Modern argument matching (Throws an informative error if typed incorrectly)
  type <- match.arg(type)
  
  # 2. Plotting with diagnostic reference lines
  if (type == "simple") {
    plot(x_scores, y_scores, xlab = "True Scores", ylab = "Estimated Scores", ...)
    # Add a red dashed 1:1 line to easily spot over/under estimation bias
    abline(a = 0, b = 1, col = "black", lty = 2, lwd = 2) 
    
  } else if (type == "abs.diff") {
    plot(x_scores, abs(y_scores - x_scores), xlab = "True Scores", ylab = "Absolute Error", ...)
    # Add a red dashed baseline at 0 error
    abline(h = 0, col = "black", lty = 2, lwd = 2) 
  }
}