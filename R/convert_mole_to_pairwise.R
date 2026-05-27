#' Convert Most/Least (MOLE) Survey Data into Pairwise Binary Outcomes
#'
#' @param data A data frame with exactly (2 * n_blocks) columns. 
#'        Odd columns = "Most" choice, Even columns = "Least" choice.
#'        Values should be numeric item IDs (either global or local block IDs).
#' @param n_blocks Integer. Number of blocks in the questionnaire.
#' @param block_size Integer. Number of items per block.
#'
#' @return A data frame of pairwise binary outcomes (1s, 0s, and NAs).
#' @export
convert_mole_to_pairwise <- function(data, n_blocks, block_size) {
  
  data <- as.data.frame(data)
  
  # Validate input format
  if (ncol(data) != 2 * n_blocks) {
    stop("Data must have exactly ", 2 * n_blocks, " columns (1 MOST and 1 LEAST column per block).")
  }
  
  pairwise_list <- list()
  
  for (b in 1:n_blocks) {
    # Extract the MOST and LEAST columns for this block
    most_col  <- data[, 2 * b - 1]
    least_col <- data[, 2 * b]
    
    # The global item names we need for the output columns (e.g., "i5", "i6")
    global_items <- ((b - 1) * block_size + 1):(b * block_size)
    
    # Auto-detect if the data contains global IDs or local IDs
    # If values are > block_size, it must be using global IDs. Otherwise, local (1 to block_size).
    max_val <- suppressWarnings(max(c(most_col, least_col), na.rm = TRUE))
    if (is.infinite(max_val)) next # Skip if the entire block is empty (all NAs)
    
    item_vals <- if (max_val > block_size) global_items else 1:block_size
    
    # Loop through all pairs in the block
    for (i in 1:(block_size - 1)) {
      for (j in (i + 1):block_size) {
        
        # Name the column perfectly for lavaan/Stan (e.g., "i1i2")
        global_i <- global_items[i]
        global_j <- global_items[j]
        col_name <- paste0("i", global_i, "i", global_j)
        
        # The values to look for in the raw data
        target_i <- item_vals[i]
        target_j <- item_vals[j]
        
        # Vectorized logical matching
        is_most_i  <- most_col == target_i
        is_least_i <- least_col == target_i
        is_most_j  <- most_col == target_j
        is_least_j <- least_col == target_j
        
        # Handle NAs safely (if respondent skipped the question)
        is_most_i[is.na(is_most_i)]   <- FALSE
        is_least_i[is.na(is_least_i)] <- FALSE
        is_most_j[is.na(is_most_j)]   <- FALSE
        is_least_j[is.na(is_least_j)] <- FALSE
        
        # Initialize pair comparison with NAs
        pair_val <- rep(NA, nrow(data))
        
        # ---------------------------------------------------------
        # The Mathematical Logic of MOLE:
        # 1. Item i wins (1) if it is MOST, or if Item j is LEAST.
        pair_val[is_most_i | is_least_j] <- 1
        
        # 2. Item j wins (0) if it is MOST, or if Item i is LEAST.
        pair_val[is_most_j | is_least_i] <- 0
        # ---------------------------------------------------------
        
        pairwise_list[[col_name]] <- pair_val
      }
    }
  }
  
  return(as.data.frame(pairwise_list))
}