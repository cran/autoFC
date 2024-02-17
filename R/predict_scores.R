#' Predict trait scores based on estimated model
#'
#' @description  An easy wrapper for the \code{thurstonianIRT::predict()} function
#'
#'
#' @param estimated_model  Estimated model
#' @param newdata  Response data from new response samples, in TIRT data format. 
#' Preferably be generated from \code{thurstonianIRT::make_TIRT_data()} or \code{get_TIRT_long_data()}.
#' @param output_file  Character string. If specified, output the trait scores into a specified csv file.
#'
#' @returns A data frame containing estimated trait scores of the new response sample
#'
#' @author Mengtong Li
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#' @import thurstonianIRT
#' 
#' @examples
#' test_data <- triplet_example_data[1:20,]
#' block_info <- triplet_block_info
#' test_data_long <- get_TIRT_long_data(block_info = triplet_block_info, response_data = test_data,
#'                                      response_varname = build_TIRT_var_names(N_blocks = 5, 
#'                                      block_size = 3, item_name = "i"),
#'                                      block_name = "Block", item_name = "ID", 
#'                                      trait_name = "Factor", sign_name = "Keying")
#' \dontrun{
#'     test_fit <- fit_TIRT_model(test_data_long, method = "mplus")
#'     predict_scores(test_fit$fit_object, newdata = test_data[21:40,])
#' }
#'
#' @export
predict_scores <- function(estimated_model, newdata, output_file = NULL) {
   new_scores <- predict(estimated_model, newdata = newdata)
   if (!"se" %in% colnames(new_scores)) {
       converted_new_scores <- new_scores %>% pivot_wider(id_cols = .data$id, names_from = .data$trait,
                                                          values_from = c(.data$estimate))
   }
   else {
       converted_new_scores <- new_scores %>% pivot_wider(id_cols = .data$id, names_from = .data$trait,
                                                          values_from = c(.data$estimate, .data$se))
   }
   if (!is.null(output_file)){ 
       write.csv(converted_new_scores, output_file)
       print(paste0("Writing predicted trait scores to ", output_file))
   }
   return(converted_new_scores)
}