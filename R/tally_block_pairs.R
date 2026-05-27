# Internal Helper: Rapidly tally pairs in a given block matrix
tally_block_pairs <- function(block_mat, item_traits, item_keys) {
  
  pairs_list <- list()
  b_size <- ncol(block_mat)
  
  # Extract all pairs from the blocks
  for (i in 1:(b_size - 1)) {
    for (k in (i + 1):b_size) {
      tA <- item_traits[block_mat[, i]]
      tB <- item_traits[block_mat[, k]]
      kA <- item_keys[block_mat[, i]]
      kB <- item_keys[block_mat[, k]]
      
      # Alphabetize traits so A-C is treated the same as C-A
      t1 <- pmin(tA, tB)
      t2 <- pmax(tA, tB)
      
      # Determine keying match
      match_type <- ifelse(kA == kB, "equal", "mixed")
      
      # Create canonical IDs for hashing
      pairs_list[[length(pairs_list) + 1]] <- paste(t1, t2, match_type, sep = "_")
    }
  }
  
  # Return table of counts
  return(table(unlist(pairs_list)))
}