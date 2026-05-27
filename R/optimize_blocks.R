#' Automatic Item Pairing Method in Forced-Choice Test Construction
#'
#' @description Automatic construction of forced-choice tests based on the
#' Simulated Annealing algorithm. Allows items to be:
#'
#' 1. Matched in either pairs, triplets, quadruplets or blocks of any size;
#'
#' 2. Matched based on any number of item-level characteristics
#' (e.g. Social desirability, factor) based on any customized criteria;
#'
#' 3. Matched based on person-level inter-item agreement (IIA) metrics;
#'
#' 4. Optimally distributed at the global scale-level to ensure specific
#' trait-pair combinations (e.g., evenly/unevenly distributing equal and/or mixed keyed pairs)
#'
#'
#' @param blocks An \emph{n} by \emph{k} integer matrix,
#' where \emph{n} is the number of item blocks
#' and \emph{k} is the number of items per block.
#' Serves as the initial starting blocks for the automatic pairing method.
#'
#' @param total_items Integer value. How many items do we sample from
#' in order to build these \code{blocks}? Should be more than number of unique values
#' in \code{block}.
#'
#' @param temp_initial Initial temperature value. Can be left as \code{NULL} and be computed based on
#' the absolute value of initial energy of \code{blocks} (Recommended), and scaled
#' by \code{temp_eta}. In general, higher temperature represents a higher probability of
#' accepting an inferior solution.
#'
#' @param temp_eta A positive numeric value. The ratio of initial temperature to
#' initial energy of \code{blocks}, if \code{temp_initial} is not designated. Default is 0.01.
#'
#' @param temp_cooling A positive numeric value less than 1.
#' Determines the reduction rate of the temperature after each iteration. Default is 0.999.
#'
#' @param temp_stop_ratio A positive numeric value less than 1.
#' Iteration stops when the temperature drops below \code{temp_stop_ratio * temp_initial}.
#' Default is \eqn{10^-6}.
#'
#' @param item_chars An \emph{m} by \emph{r} data frame,
#' where \emph{m} is the total number of items to sample from,
#' whereas \emph{r} is the number of item characteristics.
#'
#' @param char_weights A vector of length \emph{r} with weights for each
#' item characteristic in \code{item_chars}.
#' Should provide a weight of 0 for specific characteristics
#' not of interest, such as item IDs.
#'
#' @param optim_funcs A vector of customized function names for optimizing each
#' item characteristic within each block, with length \emph{r}.
#'
#' @param n_exchange Integer value. Determines how many blocks are exchanged
#' in order to produce a new solution for each iteration.
#' Should be a value larger than 1 and less than \code{nrow(blocks)}. Default is 2.
#'
#' @param prob_new_item A value between \emph{0} and \emph{1}.
#' Probability of choosing the strategy of picking a new item,
#' when not all candidate items are used to build the FC scale. Default is 0.25.
#'
#' @param use_iia Logical. Are IIA metrics used when performing automatic pairing? Default is FALSE.
#'
#' @param response_matrix A \emph{p} by \emph{m} numeric matrix with scores of each of the
#' \emph{p} participants for the \emph{m} items. Ignored when \code{use_iia == FALSE}.
#'
#' @param iia_weights A vector of length 4 indicating weights given to each IIA metric:
#' Linearly weighted AC (Gwet, 2008; 2014); Quadratic weighted AC;
#' Linearly weighted Brennan-Prediger (BP) Index (Brennan & Prediger, 1981; Gwet, 2014);
#' Quadratic weighted BP.
#'
#' @param trait_col Character string. The name of the column in \code{item_chars} indicating the trait
#' measured by the item. Required if \code{target_dist} is used for global scale-level fit.
#'
#' @param key_col Character string. The name of the column in \code{item_chars} indicating the keying
#' direction of the item (e.g., positive/negative). Required if \code{target_dist} is used.
#'
#' @param target_dist A data frame detailing the target distribution of trait-pair combinations
#' for global scale-level fit. Must contain columns \code{trait1}, \code{trait2}, \code{match_type}
#' (e.g., "equal" or "mixed"), and \code{target} (numeric count). See \code{build_target_dist()} for
#' a helper function to generate this.
#'
#' @param scale_fit_weight Numeric value. The weight applied to the global scale-level fit penalty
#' relative to the local block-level energy. Default is 1.0. Increase this value to prioritize
#' global trait distribution over local block matching.
#'
#' @param prevent_overlap Logical. If target_dist is specified, should strict prevention of trait overlap
#' (i.e., block containing items measuring the same trait) be enforced? Default is FALSE.
#'
#' @returns A list containing:
#' \itemize{
#'   \item{\code{block_initial}:} Initial starting block
#'   \item{\code{energy_initial}:} Initial energy for \code{block_initial}
#'   \item{\code{block_final}:} Final paired block after optimization by SA
#'   \item{\code{energy_final}:} Final energy for \code{block_final}
#' }
#'
#'
#' @note
#' The essence of SA is the probablistic acceptance of solutions inferior to
#' the current state, which avoids getting stuck in local maxima/minima.
#' It is also recommended to try out different values of
#' \code{char_weights, iia_weights, temp_eta} to find out the best
#' combination of initial temperature and energy value
#' in order to provide optimally paired blocks.
#'
#' @author Mengtong Li
#'
#'
#'
#' @references
#' Brennan, R. L., & Prediger, D. J. (1981). Coefficient kappa: Some uses, misuses,
#' and alternatives. \emph{Educational and Psychological Measurement, 41}(3),
#' 687-699. https://doi.org/10.1177/001316448104100307
#'
#' Gwet, K. L. (2008). Computing inter rater reliability and its
#' variance in the presence of high agreement.
#' \emph{British Journal of Mathematical and Statistical Psychology, 61}(1),
#' 29-48. https://doi.org/10.1348/000711006X126600
#'
#' Gwet, K. L. (2014). \emph{Handbook of inter-rater reliability (4th ed.):
#' The definitive guide to measuring the extent of agreement among raters}.
#' Gaithersburg, MD: Advanced Analytics Press.
#'
#' @importFrom stats runif
#'
#' @export

optimize_blocks <- function(blocks = NULL,
                            total_items = NULL,
                            temp_initial = NULL,
                            temp_eta = 0.01,
                            temp_cooling = 0.999,
                            temp_stop_ratio = 1e-6,
                            item_chars,
                            char_weights = NULL,
                            optim_funcs = NULL,
                            n_exchange = 2,
                            prob_new_item = 0.25,
                            use_iia = FALSE,
                            response_matrix = NULL,
                            iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                            # Global Scale-Level Fit Arguments
                            trait_col = NULL,
                            key_col = NULL,
                            target_dist = NULL,
                            scale_fit_weight = 1.0,
                            prevent_overlap = FALSE) {

  count_trait_overlaps <- function(block_mat, item_traits) {
    overlaps <- 0
    b_size <- ncol(block_mat)
    if (b_size < 2) return(0)

    for (i in 1:(b_size - 1)) {
      for (j in (i + 1):b_size) {
        overlaps <- overlaps + sum(item_traits[block_mat[, i]] == item_traits[block_mat[, j]])
      }
    }
    return(overlaps)
  }

  # -------------------------------------------------------------------------
  # PRE-COMPILATION FOR FAST ENERGY CALCULATION
  # -------------------------------------------------------------------------
  # 1. Convert the item_chars data.frame into a simple List of Vectors.
  char_list <- as.list(item_chars)

  # 2. Pre-match the function strings into actual R function objects
  if (is.null(optim_funcs)) {
    types <- sapply(item_chars, class)
    types[types %in% c("factor", "character")] <- "facfun"
    types[types == "numeric"] <- "var"
    optim_funcs <- types
  }
  fun_list <- lapply(optim_funcs, match.fun)


  # -------------------------------------------------------------------------
  # 1. Input Validation and Defaults
  # -------------------------------------------------------------------------
  if (is.null(blocks)) {
    blocks <- make_random_block(nrow(item_chars), block_size = 2)
  }
  if (is.null(optim_funcs)) {
    types <- sapply(item_chars, class)
    types[types %in% c("factor", "character")] <- "facfun"
    types[types == "numeric"] <- "var"
    optim_funcs <- types
  }
  if (is.null(char_weights)) char_weights <- rep(1, ncol(item_chars))
  if (is.null(total_items)) total_items <- length(unique(as.vector(blocks)))

  if (temp_cooling >= 1 || temp_cooling < 0) stop("Invalid temp_cooling: Should be between 0 and 1.")
  if (temp_stop_ratio >= 1 || temp_stop_ratio < 0) stop("Invalid temp_stop_ratio: Should be between 0 and 1.")
  if (use_iia && is.null(response_matrix)) stop("response_matrix required if use_iia = TRUE.")
  if (prob_new_item > 1 || prob_new_item < 0) stop("Invalid prob_new_item: Should be between 0 and 1.")
  if (temp_eta < 0) stop("Invalid temp_eta: Should be > 0.")
  if (n_exchange %% 1 != 0 || n_exchange > nrow(blocks) || n_exchange < 2) {
    stop("Invalid n_exchange: Must be an integer between 2 and nrow(blocks)")
  }

  # -------------------------------------------------------------------------
  # 2. Pre-Compile Energy Function (Local Block-Level Fit)
  # -------------------------------------------------------------------------
  if (use_iia) {
    calc_energy <- function(b) cal_block_energy_with_iia(b, char_list, char_weights, fun_list, response_matrix, iia_weights)
  } else {
    calc_energy <- function(b) cal_block_energy(b, char_list, char_weights, fun_list)
  }

  # -------------------------------------------------------------------------
  # 3. Initialize Global Scale-Level Fit Tracking
  # -------------------------------------------------------------------------
  use_global_fit <- !is.null(target_dist) && !is.null(trait_col) && !is.null(key_col)

  if (!is.null(target_dist) && (is.null(trait_col) || is.null(key_col))) {
    stop("Error: Both 'trait_col' and 'key_col' must be specified when using 'target_dist' for global scale-level fit.")
  }

  # if (use_global_fit && prevent_overlap) {
  #   if (is.null(trait_col)) stop("trait_col must be specified if prevent_overlap = TRUE and you wish to use a global fit.")
  #   if (is.null(key_col)) stop("key_col must be specified if prevent_overlap = TRUE and you wish to use a global fit.")
  # }

  if (use_global_fit) {
    item_traits <- as.character(item_chars[[trait_col]])
    item_keys   <- as.character(item_chars[[key_col]])

    # Pre-Optimization Diagnostic Check
    check_target_feasibility(target_dist, item_chars, trait_col, key_col,
                             n_blocks = nrow(blocks), block_size = ncol(blocks))

    # Convert user's flexible data frame into our internal fast-hashing format
    t1 <- pmin(target_dist$trait1, target_dist$trait2)
    t2 <- pmax(target_dist$trait1, target_dist$trait2)
    target_names <- paste(t1, t2, target_dist$match_type, sep = "_")

    target_vec <- target_dist$target
    names(target_vec) <- target_names

    # Calculate initial global pair distribution
    global_tally <- tally_block_pairs(blocks, item_traits, item_keys)

    # Closure to calculate Negative Mean Absolute Error (MAE) from target
    calc_global_penalty <- function(current_tally) {
      # Extract only the counts for the targeted combinations
      actual_counts <- current_tally[names(target_vec)]

      # If a target combination doesn't exist in the current block, it's an NA. Set to 0.
      actual_counts[is.na(actual_counts)] <- 0

      return(-sum(abs(actual_counts - target_vec)))
    }

    global_penalty <- calc_global_penalty(global_tally)
  }

  # -------------------------------------------------------------------------
  # 4. Initialization of Simulated Annealing
  # -------------------------------------------------------------------------
  block0 <- blocks
  energy0 <- calc_energy(blocks)
  energy <- energy0

  if (is.null(temp_initial)) {
    temp_initial <- temp_eta * abs(energy0)
  }

  T0 <- temp_initial
  end_temp <- temp_stop_ratio * T0

  # State tracking for unused items (O(1) dynamic replacement later)
  unused_items <- setdiff(seq_len(total_items), as.vector(blocks))
  all_item_used <- length(unused_items) == 0

  l <- n_exchange * ncol(blocks)

  # We use the log of the temperatures so the progress bar fills up
  # smoothly and linearly over time.
  log_T0 <- log(T0)
  log_end <- log(end_temp)
  total_distance <- log_T0 - log_end

  message("Optimizing forced-choice blocks via Simulated Annealing...")
  pb <- txtProgressBar(min = 0, max = 1, style = 3, width = 50)

  # We only update the progress bar every 100 iterations to save CPU time.
  iter_count <- 0

  # -------------------------------------------------------------------------
  # 5. Simulated Annealing Optimization Loop (Gated Priority System)
  # -------------------------------------------------------------------------
  while (temp_initial > end_temp) {

    strategy_a <- FALSE

    # --- PROPOSE A SWAP ---
    if (!all_item_used && runif(1) < prob_new_item) {
      strategy_a <- TRUE

      sample_index <- sample.int(nrow(blocks), 1)
      sample_block <- blocks[sample_index, , drop = FALSE]

      picked_idx <- sample.int(length(unused_items), 1)
      picked_item <- unused_items[picked_idx]

      exchanged_block <- sample_block
      replaced_item <- exchanged_block[1]
      exchanged_block[1] <- picked_item

    } else {
      sample_index <- sample.int(nrow(blocks), n_exchange)
      sample_block <- blocks[sample_index, , drop = FALSE]
      exchanged_block <- matrix(sample(sample_block, l), nrow = n_exchange, byrow = TRUE)
    }


    # --- 1. OVERLAP CHECK (Still a Hard Gate!) ---
    # We must absolutely forbid overlaps if the user requested it.
    if (prevent_overlap) {
      old_overlaps <- count_trait_overlaps(sample_block, item_traits)
      new_overlaps <- count_trait_overlaps(exchanged_block, item_traits)

      if (new_overlaps > old_overlaps) {
        temp_initial <- temp_initial * temp_cooling
        next
      }
      if (new_overlaps < old_overlaps) {
        blocks[sample_index, ] <- exchanged_block
        if (use_global_fit) {
          global_tally <- tally_block_pairs(blocks, item_traits, item_keys)
          global_penalty <- calc_global_penalty(global_tally)
        }
        energy <- calc_energy(blocks)
        if (strategy_a) unused_items[picked_idx] <- replaced_item
        temp_initial <- temp_initial * temp_cooling
        next
      }
    }

    # --- 2. CALCULATE UNIFIED DELTA ENERGY ---
    delta_E_total <- 0

    # Local Fit
    sample_energy <- calc_energy(sample_block)
    exchanged_energy <- calc_energy(exchanged_block)
    delta_E_local <- exchanged_energy - sample_energy

    delta_E_total <- delta_E_total + delta_E_local

    # Global Fit
    if (use_global_fit) {
      old_tally <- tally_block_pairs(sample_block, item_traits, item_keys)
      new_tally <- tally_block_pairs(exchanged_block, item_traits, item_keys)

      proposed_tally <- global_tally
      for (nm in names(old_tally)) proposed_tally[nm] <- proposed_tally[nm] - old_tally[nm]
      for (nm in names(new_tally)) {
        if (is.na(proposed_tally[nm])) proposed_tally[nm] <- 0
        proposed_tally[nm] <- proposed_tally[nm] + new_tally[nm]
      }

      proposed_penalty <- calc_global_penalty(proposed_tally)

      # The penalty is negative (e.g., -12 errors).
      # If proposed penalty is -10, delta is +2 (Improvement).
      delta_E_global <- proposed_penalty - global_penalty

      # CRITICAL: Multiply the Global Delta by the scale_fit_weight
      delta_E_total <- delta_E_total + (delta_E_global * scale_fit_weight)
    }


    # --- 3. ACCEPTANCE LOGIC ---
    # Because delta_E_total blends both local and global changes,
    # a swap that hurts local fit but massively helps global fit WILL be accepted,
    # and a swap that slightly hurts global fit but massively helps local fit MIGHT
    # be accepted if the temperature is hot enough!

    if (delta_E_total >= 0 || exp(delta_E_total / temp_initial) > runif(1)) {

      blocks[sample_index, ] <- exchanged_block
      energy <- energy + delta_E_local

      if (use_global_fit) {
        global_tally <- proposed_tally
        global_penalty <- proposed_penalty
      }
      if (strategy_a) unused_items[picked_idx] <- replaced_item
    }

    # --- END OF LOOP UPDATES ---
    temp_initial <- temp_initial * temp_cooling

    iter_count <- iter_count + 1
    if (iter_count %% 100 == 0) {
      current_distance <- log_T0 - log(temp_initial)
      setTxtProgressBar(pb, current_distance / total_distance)
    }
  }
  # Close Progress Bar
  setTxtProgressBar(pb, 1)
  close(pb)
  # -------------------------------------------------------------------------
  # 6. Return Results
  # -------------------------------------------------------------------------
  return(list(
    block_initial  = block0,
    energy_initial = energy0,
    block_final    = blocks,
    energy_final   = energy
  ))
}


# library(stats)
#
# # ==============================================================================
# # 1. SETUP: Simulate Item Pool and Initial Solution
# # ==============================================================================
# set.seed(123)
# item_dims <- rep(c("Openness", "Conscientiousness", "Neuroticism", "Extraversion", "Agreeableness"), each = 12)
# item_keys <- rep(c("1", "-1"), times = 30)
# item_mean <- rnorm(60, 5, 2)
# item_difficulty <- runif(60, -1, 1)
#
# item_df <- data.frame(Dimensions = item_dims, Mean = item_mean, Difficulty = item_difficulty, Keying = item_keys)
# solution <- make_random_block(60, 60, 3)
#
# maxmin <- function(x) {
#   return(max(x) - min(x))
# }
#
# # ------------------------------------------------------------------------------
# # RUN 1: Basic Automatic pairing (Local block-level fit only)
# # ------------------------------------------------------------------------------
# d1 <- optimize_blocks(blocks = solution, total_items = 60, temp_eta = 0.01,
#                       temp_cooling = 0.999, temp_stop_ratio = 0.001,
#                       char_weights = c(1000, -100, 1, 0), # Ignore keying in local fit
#                       item_chars = item_df, trait_col = "Dimensions",
#                       optim_funcs = c("facfun", "maxmin", "var", "facfun"),
#                       prevent_overlap = TRUE)
# print(summarize_trait_pairs(d1$block_final, item_chars = item_df, trait_col = "Dimensions", key_col = "Keying",
#                             output_format = "long"))
#
# # ==============================================================================
# # 2. SETUP: Global Target Distribution
# # ==============================================================================
# # Create a target distribution aiming for evenly distributed traits
# # with a 50/50 ratio of equal-keyed to mixed-keyed pairs.
# my_targets <- build_target_dist(traits = unique(item_dims),
#                                 total_pairs = 60, # 20 blocks * 3 pairs each
#                                 equal_mixed_ratio = c(1, 1),
#                                 allow_same_trait = FALSE)
#
# # Manually tweak the targets to create an uneven/custom requirement
# my_targets2 <- my_targets
# my_targets2$target[1:7] <- c(2, 1, 4, 2, 5, 1, 2)
# my_targets2$target[11:17] <- 6 - my_targets2$target[1:7]
#
# # ------------------------------------------------------------------------------
# # RUN 2: Automatic pairing WITH Global Scale-Level Target Distribution
# # ------------------------------------------------------------------------------
# cat("\n--- Running Optimization 2 (Global Fit) ---\n")
# d2a <- optimize_blocks(blocks = solution, total_items = 60, temp_eta = 1e-2,
#                        temp_cooling = 0.9995, temp_stop_ratio = 1e-6,
#                        char_weights = c(1, -10, 1, 0),
#                        item_chars = item_df,
#                        optim_funcs = c("facfun", "maxmin", "var", "facfun"),
#                        trait_col = "Dimensions",
#                        key_col = "Keying",
#                        target_dist = my_targets,
#                        scale_fit_weight = 50,
#                        prevent_overlap = TRUE)
#
# d2b <- optimize_blocks(blocks = solution, total_items = 60, temp_eta = 1e-2,
#                        temp_cooling = 0.9995, temp_stop_ratio = 1e-6,
#                        char_weights = c(1, -10, 1, 0),
#                        item_chars = item_df,
#                        optim_funcs = c("facfun", "maxmin", "var", "facfun"),
#                        trait_col = "Dimensions",
#                        key_col = "Keying",
#                        target_dist = my_targets2,
#                        scale_fit_weight = 50,
#                        prevent_overlap = TRUE)
#
# cat("\nDiagnostics for D2 (Global Fit):\n")
# ddd1 <- (summarize_trait_pairs(d2b$block_final, item_chars = item_df, trait_col = "Dimensions", key_col = "Keying", output_format = "long"))
#
#
# # ==============================================================================
# # 3. SETUP: Simulate Participant Responses for IIA Optimization
# # ==============================================================================
# # Simulate 500 participants responding to the 60 items on a 1-5 Likert scale
# simulated_responses <- matrix(sample(1:5, 500 * 60, replace = TRUE), nrow = 500, ncol = 60)
#
# # ------------------------------------------------------------------------------
# # RUN 3: Automatic pairing WITH Global Targets AND Inter-Item Agreement (IIA)
# # ------------------------------------------------------------------------------
# cat("\n--- Running Optimization 3 (Global Fit + IIA) ---\n")
#
# d3 <- optimize_blocks(blocks = solution, total_items = 60,
#                       temp_eta = 1e-2, temp_cooling = 0.9995, temp_stop_ratio = 1e-6,
#
#                       # Local Block Weights
#                       char_weights = c(1, -10, 1, 0),
#                       item_chars = item_df,
#                       optim_funcs = c("facfun", "maxmin", "var", "facfun"),
#                       # Global Scale Weights & Rules
#                       trait_col = "Dimensions",
#                       key_col = "Keying",
#                       target_dist = my_targets2,
#                       scale_fit_weight = 50,
#                       prevent_overlap = TRUE,
#
#                       # IIA Arguments
#                       use_iia = TRUE,
#                       response_matrix = simulated_responses,
#
#                       # Adjust IIA weights depending on how heavily you want
#                       # agreement indices to influence the block formations.
#                       iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1))
#
# cat("\nDiagnostics for D3 (Global Fit + IIA):\n")
# ddd3 <- (summarize_trait_pairs(d3$block_final, item_chars = item_df, trait_col = "Dimensions", key_col = "Keying",
#                                output_format = "long"))
# item_df[c(t(d3$block_final)),]
