#' Convert Ranked Blocks into Pairwise Binary Outcomes
#'
#' Converts a dataset of ranked forced-choice blocks into pairwise binary comparisons 
#' suitable for Thurstonian IRT modeling. 
#'
#' @param data A data frame or matrix where rows are respondents and columns are items. 
#'        The columns must be ordered by block (e.g., Block 1 Item 1, Block 1 Item 2, 
#'        Block 1 Item 3, Block 2 Item 1, etc.). Each column represents the assigned
#'        rank of that corresponding item in the block.
#' @param n_blocks Integer. Number of blocks in the questionnaire.
#' @param block_size Integer. Number of items per block.
#' @param lower_rank_is_better Logical. In your data, does a smaller number mean a 
#'        higher preference? (e.g., 1 = Most Preferred, 2 = Second Most). 
#'        Defaults to TRUE.
#'
#' @return A data frame of binary outcomes (1s, 0s, and NAs). The column names will 
#'         match the "i1i2", "i1i3" format expected by the syntax generators.
#' @export
convert_ranks_to_pairwise <- function(data, 
                                      n_blocks, 
                                      block_size, 
                                      lower_rank_is_better = TRUE) {
  
  # Ensure input is a data frame
  data <- as.data.frame(data)
  n_items <- n_blocks * block_size
  
  # Validate dimensions
  if (ncol(data) < n_items) {
    stop("The dataset has fewer columns (", ncol(data), 
         ") than n_blocks * block_size (", n_items, ").")
  }
  
  # If the user passed a dataframe with extra columns (like an ID column),
  # we isolate only the item columns to avoid referencing wrong data.
  data <- data[, 1:n_items]
  
  # Initialize an empty list to store the new pairwise columns
  pairwise_list <- list()
  
  # Loop through blocks
  for (b in 1:n_blocks) {
    # Get the global item indices for the current block
    # E.g., for block 2, block_size 3, this is 4, 5, 6
    block_items <- ((b - 1) * block_size + 1):(b * block_size)
    
    # Loop through pairs within the block
    for (i in 1:(block_size - 1)) {
      for (j in (i + 1):block_size) {
        
        item_i <- block_items[i]
        item_j <- block_items[j]
        
        # Name the column to perfectly match the lavaan/Mplus syntax
        col_name <- paste0("i", item_i, "i", item_j)
        
        # Determine pairwise preference
        if (lower_rank_is_better) {
          # If 1=Best, item_i gets a 1 if its rank is SMALLER than item_j
          pairwise_val <- as.numeric(data[, item_i] < data[, item_j])
        } else {
          # If 5=Best, item_i gets a 1 if its score is LARGER than item_j
          pairwise_val <- as.numeric(data[, item_i] > data[, item_j])
        }
        
        pairwise_list[[col_name]] <- pairwise_val
      }
    }
  }
  
  # Convert the list of columns back into a data frame and return
  pairwise_df <- as.data.frame(pairwise_list)
  return(pairwise_df)
}