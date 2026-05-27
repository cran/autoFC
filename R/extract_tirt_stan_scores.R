#' Extract Trait Scores and SEs from a Fitted Stan TIRT Model
#'
#' @param fit A fitted cmdstanr object.
#' @param stan_data The data list passed to Stan (must contain the 'trait_names' attribute).
#'
#' @return A data frame with respondents as rows, and Traits and Trait SEs as columns.
#' @export
extract_tirt_stan_scores <- function(fit, stan_data) {
  
  # 1. Pull the trait names we saved as an attribute
  trait_names <- attr(stan_data, "trait_names")
  N <- stan_data$N
  D <- stan_data$D
  
  # 2. Extract the summary for the 'theta' matrix from cmdstanr
  # (mean = the trait score, sd = the standard error)
  cat("Extracting posterior means and standard errors...\n")
  theta_summary <- fit$summary(variables = "theta", c("mean", "sd"))
  
  # 3. Initialize an empty data frame for the results
  res_df <- data.frame(matrix(NA, nrow = N, ncol = D * 2))
  
  # Name the columns cleanly (e.g., Openness, Openness_SE)
  col_names <- c()
  for (t in trait_names) col_names <- c(col_names, t, paste0(t, "_SE"))
  colnames(res_df) <- col_names
  
  # 4. Populate the data frame
  # Stan outputs 'theta[n, d]' where n is person, d is trait
  for (d in 1:D) {
    # Filter the summary table for the specific trait 'd'
    # This regex ensures we only grab theta[..., d]
    pattern <- sprintf("^theta\\[\\d+,%d\\]$", d)
    trait_rows <- theta_summary[grepl(pattern, theta_summary$variable), ]
    
    # Insert into the final data frame
    res_df[, (d - 1) * 2 + 1] <- trait_rows$mean  # The Score
    res_df[, (d - 1) * 2 + 2] <- trait_rows$sd    # The Standard Error
  }
  
  return(res_df)
}