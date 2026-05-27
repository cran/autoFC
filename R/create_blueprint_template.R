#' Generate a Blueprint Template for Forced-Choice Scales
#'
#' @description Generates a structured data frame representing a forced-choice 
#' test blueprint. Optionally exports it directly to a CSV file so test developers 
#' can manually design their block structures (assigning specific traits and keying) 
#' in spreadsheet software.
#'
#' @param n_blocks Integer. Number of total FC blocks.
#' @param block_size Integer. Desired block size for the FC scale.
#' @param file_path Character. Optional. If provided, the template will be saved 
#'        to this path as a CSV file (e.g., \code{"my_blueprint.csv"}).
#' 
#' @returns A data frame containing the \code{block} and \code{item_num} columns, 
#'          plus empty columns for \code{trait} and \code{sign} ready to be filled.
#' 
#' @export
create_blueprint_template <- function(n_blocks, block_size, file_path = NULL) {
  
  output_bp <- data.frame(
    block = rep(1:n_blocks, each = block_size),
    item_num = rep(1:block_size, times = n_blocks),
    trait = NA,
    sign = NA
  )
  
  if (!is.null(file_path)) {
    write.csv(output_bp, file = file_path, row.names = FALSE, na = "")
    message("Blueprint template successfully saved to: ", file_path)
  }
  
  return(output_bp)
}