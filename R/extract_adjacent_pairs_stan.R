#' Extract Only Adjacent Rank Pairs for Stan (Heister et al., 2025)
#'
#' @param raw_ranks Dataframe of raw rankings (1 = Best).
#' @param n_blocks Integer.
#' @param block_size Integer.
#' @return A long-format list ready to inject directly into the Stan data list.
#' @export
extract_adjacent_pairs_stan <- function(raw_ranks, n_blocks, block_size) {
  
  long_person <- c()
  long_pair <- c()
  long_y <- c()
  
  # Helper to find the global pair index for any two items
  get_pair_idx <- function(item1, item2, b) {
    start_pair <- (b - 1) * (block_size * (block_size - 1) / 2)
    i_local <- (min(item1, item2) - 1) %% block_size + 1
    k_local <- (max(item1, item2) - 1) %% block_size + 1
    
    # Mathematical mapping to pair index
    offset <- 0
    if (i_local > 1) {
      for(step in 1:(i_local - 1)) offset <- offset + (block_size - step)
    }
    return(start_pair + offset + (k_local - i_local))
  }
  
  for (n in 1:nrow(raw_ranks)) {
    for (b in 1:n_blocks) {
      # Get items for this block
      items_idx <- ((b - 1) * block_size + 1):(b * block_size)
      person_ranks <- as.numeric(raw_ranks[n, items_idx])
      
      # Order the items by their rank (1st, 2nd, 3rd...)
      ranked_items <- items_idx[order(person_ranks)]
      
      # ONLY pull adjacent comparisons (1 vs 2, 2 vs 3)
      for (r in 1:(block_size - 1)) {
        item_better <- ranked_items[r]
        item_worse  <- ranked_items[r + 1]
        
        pair_id <- get_pair_idx(item_better, item_worse, b)
        
        # If the actual pair was written as (smaller_id, larger_id), y = 1
        # If it was written as (larger_id, smaller_id), y = 0
        y_val <- ifelse(item_better < item_worse, 1, 0)
        
        long_person <- c(long_person, n)
        long_pair <- c(long_pair, pair_id)
        long_y <- c(long_y, y_val)
      }
    }
  }
  
  return(list(person = long_person, pair = long_pair, y = long_y))
}