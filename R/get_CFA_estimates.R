#' Conduct Confirmatory Factor Analysis (CFA) and Obtain Parameter Estimates
#'
#' @description Performs CFA (or accepts an existing fitted model) and extracts 
#' the parameter estimates required for generating simulated Thurstonian IRT data.
#'
#' @param fit_model Either a character string containing `lavaan` syntax, OR a 
#'        pre-fitted `lavaan` object.
#' @param response_data Optional. Data frame containing Likert-type responses. 
#'        Required only if \code{fit_model} is a syntax string.
#' @param item_names Optional character vector. Names of the items. If \code{NULL}, 
#'        the function automatically detects the observed variables from the model.
#'
#' @returns A list containing \code{loadings}, \code{intercepts}, \code{residuals}, 
#'          \code{covariances}, and the full \code{model_fit} object.
#' @export
get_CFA_estimates <- function(fit_model, response_data = NULL, item_names = NULL) {
  
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("The 'lavaan' package is required.")
  }
  
  # 1. Fit the model (or accept an already fitted model)
  if (inherits(fit_model, "lavaan")) {
    fit_object <- fit_model
  } else {
    if (is.null(response_data)) stop("response_data is required to fit the model syntax.")
    fit_object <- lavaan::cfa(fit_model, data = response_data, std.lv = TRUE, meanstructure = TRUE)
  }
  
  # 2. Auto-detect item names if not provided
  if (is.null(item_names)) {
    item_names <- lavaan::lavNames(fit_object, "ov")
  }
  
  # 3. Extract parameter estimates (Base R replaces dplyr)
  est <- lavaan::parameterEstimates(fit_object)
  
  item_loadings <- est[est$op == "=~" & est$rhs %in% item_names, ]
  item_intercepts <- est[est$op == "~1" & est$lhs %in% item_names, ]
  item_residuals <- est[est$op == "~~" & est$lhs %in% item_names & est$lhs == est$rhs, ]
  
  lv_covariances <- as.matrix(lavaan::lavInspect(fit_object, "cov.lv"))
  
  return(list(
    loadings = item_loadings, 
    intercepts = item_intercepts,
    residuals = item_residuals, 
    covariances = lv_covariances, 
    model_fit = fit_object
  ))     
}
