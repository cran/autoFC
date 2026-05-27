#' Calculate Classical Ipsative (Sum) Scores for Forced-Choice Measures
#'
#' @description Calculates traditional ipsative sum scores from pairwise binary 
#' outcomes. Natively handles both full rankings and partial rankings (MOLE format) 
#' via pro-rated scaling, and automatically reverses scores for negatively-keyed items.
#'
#' @param pairwise_data A data frame of binary pairwise outcomes (output of 
#'        \code{convert_ranks_to_pairwise} or \code{convert_mole_to_pairwise}).
#' @param n_blocks Integer. Number of blocks in the questionnaire.
#' @param block_size Integer. Number of items per block.
#' @param key_matrix A data.frame with two columns, one indicating item trait,
#'        another indicating item sign.
#' @param trait_col Character string. The name of the column in \code{key_matrix} indicating the trait
#'        measured by the item. 
#' @param key_col Character string. The name of the column in \code{key_matrix} indicating the keying 
#'        direction of the item (e.g., positive/negative). 
#'
#' @return A data frame of classical ipsative scores for each unique trait, 
#'         with rows representing respondents.
#' @export
score_tirt_ipsative <- function(pairwise_data, n_blocks, block_size, key_matrix, trait_col, key_col) {
  
  n_items <- n_blocks * block_size
  n_pairs <- n_blocks * (block_size * (block_size - 1) / 2)
  N <- nrow(pairwise_data)
  
  if (ncol(pairwise_data) != n_pairs) {
    stop("The number of columns in 'pairwise_data' does not match the expected number of pairs.")
  }
  
  # --- 1. Build Pairs Map ---
  Pairs <- matrix(0, nrow = n_pairs, ncol = 2)
  pair_idx <- 1
  for (b in 1:n_blocks) {
    for (i in 1:(block_size - 1)) {
      for (k in (i + 1):block_size) {
        Pairs[pair_idx, 1] <- (b - 1) * block_size + i
        Pairs[pair_idx, 2] <- (b - 1) * block_size + k
        pair_idx <- pair_idx + 1
      }
    }
  }
  
  # --- 2. Calculate Item-Level Wins & Observed Comparisons ---
  # We pre-allocate matrices to perform fast vectorized math
  item_wins <- matrix(0, nrow = N, ncol = n_items)
  item_obs  <- matrix(0, nrow = N, ncol = n_items)
  
  # Loop over columns (Pairs), which is extremely fast even for huge N
  for (p in 1:n_pairs) {
    col_data <- pairwise_data[, p]
    item_i <- Pairs[p, 1]
    item_k <- Pairs[p, 2]
    
    # Track non-missing comparisons (to support MOLE pro-rating)
    is_valid <- !is.na(col_data)
    item_obs[, item_i] <- item_obs[, item_i] + is_valid
    item_obs[, item_k] <- item_obs[, item_k] + is_valid
    
    # If 1, Item I won. If 0, Item K won.
    item_wins[, item_i] <- item_wins[, item_i] + ifelse(is_valid & col_data == 1, 1, 0)
    item_wins[, item_k] <- item_wins[, item_k] + ifelse(is_valid & col_data == 0, 1, 0)
  }
  
  # --- 3. Pro-Rate Scores (Saves MOLE data from being underestimated) ---
  # If no missing data, this is mathematically identical to just 'item_wins'
  item_scores <- (item_wins / item_obs) * (block_size - 1)
  item_scores[is.nan(item_scores)] <- 0 # Handle division by zero for totally skipped items
  
  # --- 4. Aggregate to Trait-Level Scores (Handling Keying) ---
  trait_names <- unique(as.character(key_matrix[[trait_col]]))
  n_traits <- length(trait_names)
  
  ipsative_scores <- data.frame(matrix(0, nrow = N, ncol = n_traits))
  colnames(ipsative_scores) <- trait_names
  
  for (t_idx in 1:n_traits) {
    t_name <- trait_names[t_idx]
    items_for_t <- which(as.character(key_matrix[[trait_col]]) == t_name)
    
    trait_sum <- rep(0, N)
    for (i in items_for_t) {
      sign_val <- key_matrix[[key_col]][i]
      
      if (sign_val == 1) {
        # Positive item: More wins = Higher trait
        trait_sum <- trait_sum + item_scores[, i]
      } else {
        # Negative item: Fewer wins = Higher trait (Flipped scoring)
        trait_sum <- trait_sum + ((block_size - 1) - item_scores[, i])
      }
    }
    ipsative_scores[, t_name] <- trait_sum
  }
  
  return(ipsative_scores)
}