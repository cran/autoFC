#' Build Forced-Choice Blocks from a Custom Blueprint
#'
#' @description Constructs an initial set of forced-choice blocks that strictly
#' adhere to a user-specified blueprint (e.g., exact trait and keying combinations
#' for every block). It utilizes a fast random-search heuristic to simultaneously
#' optimize a specified matching criterion (e.g., minimizing within-block difficulty variance).
#'
#' This function acts as an excellent "Smart Initializer" before passing the
#' resulting blocks into \code{assemble_blocks()} for further global optimization.
#'
#' @param item_chars A data frame containing the item pool characteristics. Each row
#'        represents a single item.
#' @param blueprint A data frame detailing the exact structural requirements for every block.
#'        Must contain columns mapping to the block ID, required trait, and required keying direction.
#' @param bp_block_col Character string. The column name in \code{blueprint} identifying the block ID.
#'        Defaults to \code{"block"}.
#' @param bp_trait_col Character string. The column name in \code{blueprint} identifying the required trait.
#'        Defaults to \code{"trait"}.
#' @param bp_sign_col Character string. The column name in \code{blueprint} identifying the required keying sign.
#'        Defaults to \code{"sign"}.
#' @param bp_crit_col Character string. Optional. The column name in \code{blueprint} defining the initial
#'        numerical target for the matching criterion (e.g., maximum allowed difference).
#'        Defaults to \code{NULL}.
#' @param df_id_col Character string.  The column name in \code{item_chars} containing item number.
#' @param df_trait_col Character string. The column name in \code{item_chars} containing the items' traits.
#'        Defaults to \code{"trait"}.
#' @param df_sign_col Character string. The column name in \code{item_chars} containing the items' keying signs.
#'        Defaults to \code{"sign"}.
#' @param df_crit_col Character string. Optional. The column name in \code{item_chars} containing the numerical
#'        values to be optimized (e.g., item difficulty). Defaults to \code{NULL}.
#' @param optim_func Character string or function object. Optional. The function used to evaluate the
#'        items within a block (e.g., \code{"var"} or a custom function). The result is
#'        compared against the target in \code{bp_crit_col}. Defaults to \code{NULL}.
#' @param adjust_factor Numeric value > 1. The multiplier used to relax the matching criterion target
#'        if a valid combination cannot be found. For example, \code{1.1} increases the allowed variance
#'        by 10 percents per failure. Defaults to \code{1.1}.
#' @param max_comb_attempts Integer. The maximum number of random combinations to generate and test
#'        against the matching criterion before relaxing the target. Defaults to \code{100}.
#' @param max_adjust_attempts Integer. The maximum number of times to relax the target criterion using
#'        \code{adjust_factor} before giving up on the block. Defaults to \code{5}.
#'
#' @return A data frame containing the successfully matched items. The data frame will include
#'         all original columns from \code{item_chars}, plus tracking columns \code{N_adjusts},
#'         \code{final_criteria}, and \code{block_id}.
#'         If the algorithm fails to construct a block due to depleted inventory or overly strict
#'         criteria, it returns the partially built scale up to the point of failure.
#'
#' @export


build_blueprint_blocks <- function(item_chars,
                                   blueprint,
                                   bp_block_col = "block",
                                   bp_trait_col = "trait",
                                   bp_sign_col = "sign",
                                   bp_crit_col = NULL,
                                   df_id_col = "item_num",
                                   df_trait_col = "trait",
                                   df_sign_col = "sign",
                                   df_crit_col = NULL,
                                   optim_func = NULL,
                                   adjust_factor = 1.1,
                                   max_comb_attempts = 100,
                                   max_adjust_attempts = 5) {

  # 1. Initialization
  n_items <- nrow(item_chars)
  used_items <- rep(FALSE, n_items) # Fast boolean tracker for O(1) lookups

  block_ids <- unique(blueprint[[bp_block_col]])
  n_blocks <- length(block_ids)

  # Pre-allocate list to prevent the `rbind` memory loop
  out_list <- vector("list", n_blocks)

  # Safely bind the function object once
  if (!is.null(optim_func)) {
    fun <- match.fun(optim_func)
  }

  # 2. Iterate through blocks
  for (b_idx in seq_along(block_ids)) {
    b_id <- block_ids[b_idx]
    current_bp <- blueprint[blueprint[[bp_block_col]] == b_id, ]
    block_size <- nrow(current_bp)

    # Base criterion for this block
    current_crit <- if (!is.null(bp_crit_col)) current_bp[[bp_crit_col]][1] else NA

    block_success <- FALSE
    n_adjusts <- 0

    # 3. Relaxation Loop
    while (n_adjusts <= max_adjust_attempts && !block_success) {

      # 4. Fast Random Search Loop (Replaces expand.grid)
      for (atm in 1:max_comb_attempts) {

        picked_idx <- integer(block_size)
        valid_sample <- TRUE

        # Clone the used_items tracker so we don't accidentally sample the same item
        # twice in the same block if the blueprint requires duplicate trait/signs
        temp_used <- used_items

        for (slot in 1:block_size) {
          # Find all available items for this specific slot's requirements
          pool <- which(item_chars[[df_trait_col]] == current_bp[[bp_trait_col]][slot] &
                          item_chars[[df_sign_col]] == current_bp[[bp_sign_col]][slot] &
                          !temp_used)

          # If pool is empty, this random path failed. Try again.
          if (length(pool) == 0) {
            valid_sample <- FALSE
            break
          }

          # Safe random sampling
          chosen <- if (length(pool) == 1) pool else sample(pool, 1)

          picked_idx[slot] <- chosen
          temp_used[chosen] <- TRUE # Mark as temporarily used
        }

        if (!valid_sample) next

        # 5. Check Optimization Criterion
        if (is.null(optim_func)) {
          block_success <- TRUE
          break
        } else {
          val <- fun(item_chars[[df_crit_col]][picked_idx])
          if (val <= current_crit) {
            block_success <- TRUE
            break
          }
        }
      }

      # If successful, break out of relaxation loop
      if (block_success) break

      # Relax criterion and try again
      n_adjusts <- n_adjusts + 1
      if (!is.na(current_crit)) current_crit <- current_crit * adjust_factor
    }

    # 6. Handle Success or Failure
    if (block_success) {
      # Permanently mark items as used
      used_items[picked_idx] <- TRUE

      # Build the dataframe for this block
      matched_df <- item_chars[picked_idx, , drop = FALSE]
      matched_df$N_adjusts <- if (is.null(optim_func)) "Not Applicable" else n_adjusts
      matched_df$final_criteria <- if (is.null(optim_func)) "Not specified" else current_crit
      matched_df$block_id <- b_id

      out_list[[b_idx]] <- matched_df

    } else {
      # Failure: Hit the greedy trap or impossible constraints
      warning(sprintf("Failed to build Block %s. \nThe item pool is depleted for the required trait, or the matching criterion is too strict.\nReturning a partially built scale.", b_id), call. = FALSE)

      # Drop empty slots and return the partial scale
      out_list <- out_list[!sapply(out_list, is.null)]
      if (length(out_list) > 0) return(do.call(rbind, out_list)) else return(data.frame())
    }
  }

  # Return the perfectly built scale!
  return(do.call(rbind, out_list))
}
