#' Create Target Pair Distribution for Scale-Level Fit
#'
#' @param traits A character vector of unique trait names (e.g., c("O", "C", "E", "A", "N")).
#' @param total_pairs Integer. The total number of pairs your test will generate.
#' @param equal_mixed_ratio Numeric vector of length 2. The desired ratio of 
#'        "equal" keyed pairs to "mixed" keyed pairs (e.g., c(1, 1) for 50/50 split).
#' @param allow_same_trait Logical. Do we allow items measuring the same traits to be paired together?
#' @return A flexible data frame containing target counts for the SA algorithm.
#' @export
build_target_dist <- function(traits, total_pairs, equal_mixed_ratio = c(1, 1), allow_same_trait = FALSE) {
  
  traits <- sort(unique(traits))
  
  # 1. Generate combinations
  if (allow_same_trait) {
    # expand.grid gets ALL combinations, including self-matches
    trait_combs <- expand.grid(trait1 = traits, trait2 = traits, stringsAsFactors = FALSE)
    # Sort them row-wise to remove duplicates (e.g., O-C and C-O become C-O)
    trait_combs <- as.data.frame(t(apply(trait_combs, 1, sort)), stringsAsFactors = FALSE)
    trait_combs <- unique(trait_combs)
    colnames(trait_combs) <- c("trait1", "trait2")
  } else {
    trait_combs <- as.data.frame(t(combn(traits, 2)), stringsAsFactors = FALSE)
    colnames(trait_combs) <- c("trait1", "trait2")
  }
  
  # 2. Add 'match_type'
  match_types <- data.frame(match_type = c("equal", "mixed"), stringsAsFactors = FALSE)
  dist_df <- merge(trait_combs, match_types, by = NULL)
  
  # Note: A same-trait pair (O-O) that is "mixed" keying means one item is Positive Openness 
  # and the other is Negative Openness. This is valid.
  # A same-trait pair that is "equal" keying means both are Positive Openness. 
  
  # 3. Distribute targets
  n_combs <- nrow(trait_combs) 
  ratio_prop <- equal_mixed_ratio / sum(equal_mixed_ratio)
  
  dist_df$target <- ifelse(dist_df$match_type == "equal",
                           (total_pairs * ratio_prop[1]) / n_combs,
                           (total_pairs * ratio_prop[2]) / n_combs)
  
  return(dist_df)
}
