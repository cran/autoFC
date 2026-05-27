#' Internal Function: Check Feasibility of Global Target Distribution
#' @noRd
check_target_feasibility <- function(target_dist, item_chars, trait_col, key_col, n_blocks, block_size) {

  issues <- c()

  total_test_pairs <- n_blocks * (block_size * (block_size - 1) / 2)
  target_sum <- sum(target_dist$target)

  # 1. Check Total Pairs
  if (target_sum > total_test_pairs) {
    issues <- c(issues, sprintf("-> IMPOSSIBLE TOTAL: Target asks for %s pairs, but test only generates %s.",
                                round(target_sum, 1), total_test_pairs))
  } else if (target_sum < total_test_pairs) {
    message(sprintf("Note: Partial constraints detected. Optimizing %s targeted pairs out of %s total.",
                    round(target_sum, 1), total_test_pairs))
  }

  # 2. Check MARGINAL Trait Requirements (The Sudoku Check)
  # In a block of k, an item is compared to (k-1) other items.
  # Thus, 1 item generates (k-1) pairs for its trait.
  trait_pairs_needed <- rep(0, length(unique(item_chars[[trait_col]])))
  names(trait_pairs_needed) <- unique(item_chars[[trait_col]])

  for (i in 1:nrow(target_dist)) {
    t1 <- target_dist$trait1[i]
    t2 <- target_dist$trait2[i]
    tgt <- target_dist$target[i]

    if (t1 %in% names(trait_pairs_needed)) trait_pairs_needed[t1] <- trait_pairs_needed[t1] + tgt
    if (t2 %in% names(trait_pairs_needed)) trait_pairs_needed[t2] <- trait_pairs_needed[t2] + tgt
  }

  items_needed <- trait_pairs_needed / (block_size - 1)

  # Compare against actual pool
  for (tr in names(items_needed)) {
    pool_count <- sum(item_chars[[trait_col]] == tr)
    if (items_needed[tr] > pool_count) {
      issues <- c(issues, sprintf("-> IMPOSSIBLE MARGINAL: Target requires %s '%s' items, but pool only has %s.",
                                  ceiling(items_needed[tr]), tr, pool_count))
    }
  }

  # 3. Check Pairwise supply limits
  pool_tab <- table(item_chars[[trait_col]], item_chars[[key_col]])
  keys <- colnames(pool_tab); traits <- rownames(pool_tab)
  get_n <- function(t, k) if (t %in% traits && k %in% keys) return(pool_tab[t, k]) else return(0)

  for (i in 1:nrow(target_dist)) {
    t1 <- target_dist$trait1[i]; t2 <- target_dist$trait2[i]
    mtype <- target_dist$match_type[i]; tgt <- target_dist$target[i]

    max_possible <- 0
    if (mtype == "equal") {
      for (k in keys) max_possible <- max_possible + min(get_n(t1, k), get_n(t2, k))
    } else if (mtype == "mixed") {
      for (k1 in keys) {
        for (k2 in keys) {
          if (k1 != k2) max_possible <- max_possible + min(get_n(t1, k1), get_n(t2, k2))
        }
      }
    }

    if (tgt > max_possible) {
      issues <- c(issues, sprintf("-> IMPOSSIBLE PAIR: %s-%s (%s) requests %s pairs, but pool only allows max %s.",
                                  t1, t2, mtype, round(tgt, 1), max_possible))
    }
  }

  if (length(issues) > 0) {
    warning("Global Scale-Fit Issues Detected:\n", paste(issues, collapse = "\n"),
            "\n\nNOTE: SA algorithm will continue and return the closest possible fit.")
  }
}
