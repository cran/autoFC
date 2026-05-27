#' Prepare pairwise list data for Stan
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
#' @param apply_heister Logical. Should the optimization techinques proposed by Heister et al., (2025) be applied?
#'        Default is FALSE.
#' @export
prepare_tirt_stan_data <- function(pairwise_data, n_blocks, block_size, key_matrix, 
                                   trait_col, key_col, apply_heister = FALSE) {
  
  N <- nrow(pairwise_data)
  K <- n_blocks * block_size
  P <- n_blocks * (block_size * (block_size - 1) / 2)
  
  # Map string traits to numeric factors for Stan (1 to D)
  trait_names <- unique(as.character(key_matrix[[trait_col]]))
  D <- length(trait_names)
  numeric_traits <- as.numeric(factor(as.character(key_matrix[[trait_col]]), levels = trait_names))
  
  item_i <- c(); item_k <- c()
  for (b in 1:n_blocks) {
    for (i in 1:(block_size - 1)) {
      for (k in (i + 1):block_size) {
        item_i <- c(item_i, (b - 1) * block_size + i)
        item_k <- c(item_k, (b - 1) * block_size + k)
      }
    }
  }
  
  long_person <- c(); long_pair <- c(); long_y <- c()
  
  for (n in 1:N) {
    for (b in 1:n_blocks) {
      start_pair <- (b - 1) * (block_size * (block_size - 1) / 2) + 1
      end_pair <- b * (block_size * (block_size - 1) / 2)
      block_pairs <- start_pair:end_pair
      
      y_block <- as.numeric(pairwise_data[n, block_pairs])
      
      if (any(is.na(y_block)) || !apply_heister || block_size <= 2) {
        valid_idx <- which(!is.na(y_block))
        if (length(valid_idx) > 0) {
          long_person <- c(long_person, rep(n, length(valid_idx)))
          long_pair   <- c(long_pair, block_pairs[valid_idx])
          long_y      <- c(long_y, y_block[valid_idx])
        }
      } else {
        # Heister Logical Reduction
        wins <- rep(0, block_size)
        idx <- 1
        for (i in 1:(block_size - 1)) {
          for (k in (i + 1):block_size) {
            if (y_block[idx] == 1) wins[i] <- wins[i] + 1 else wins[k] <- wins[k] + 1
            idx <- idx + 1
          }
        }
        if (length(unique(wins)) == block_size) {
          idx <- 1
          for (i in 1:(block_size - 1)) {
            for (k in (i + 1):block_size) {
              if (abs(wins[i] - wins[k]) == 1) {
                long_person <- c(long_person, n); long_pair <- c(long_pair, block_pairs[idx]); long_y <- c(long_y, y_block[idx])
              }
              idx <- idx + 1
            }
          }
        } else {
          long_person <- c(long_person, rep(n, length(y_block))); long_pair <- c(long_pair, block_pairs); long_y <- c(long_y, y_block)
        }
      }
    }
  }
  
  is_fixed_psi <- rep(0, K); fixed_psi_val <- rep(0, K)
  if (block_size == 2) {
    is_fixed_psi <- rep(1, K); fixed_psi_val <- rep(0.5, K)
  } else {
    for (b in 1:n_blocks) {
      first_item <- (b - 1) * block_size + 1
      is_fixed_psi[first_item] <- 1; fixed_psi_val[first_item] <- 1.0
    }
  }
  free_psi_idx <- which(is_fixed_psi == 0)
  
  # -------------------------------------------------------------
  # FIX: Build the strict list for Stan
  # -------------------------------------------------------------
  stan_data <- list(
    N = N, K = K, P = P, D = D, N_obs = length(long_y),
    person = long_person, pair = long_pair, y = long_y,
    item_i = item_i, item_k = item_k,
    trait = numeric_traits, item_sign = key_matrix[[key_col]],
    K_free = length(free_psi_idx), is_fixed_psi = is_fixed_psi,
    fixed_psi_val = fixed_psi_val, free_psi_idx = free_psi_idx
  )
  
  # Attach the trait names as a hidden attribute so it doesn't crash Stan!
  attr(stan_data, "trait_names") <- trait_names
  
  return(stan_data)
}
