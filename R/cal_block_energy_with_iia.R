#' Fast Calculation of Item Block "Energy" with Inter-Item Agreement
#'
#' @description Calculates local block energy including IIA metrics. Heavily 
#' optimized for Simulated Annealing.
#'
#' @param block An \emph{n} by \emph{k} integer matrix of item indices.
#' @param char_list A list of vectors for item characteristics.
#' @param weights A numeric vector of weights for item characteristics.
#' @param fun_list A list of pre-matched R function objects.
#' @param rater_chars A matrix of participant responses.
#' @param iia_weights A numeric vector of length 4 for IIA metrics.
#' @param verbose Logical. If TRUE, prints metrics. Defaults to FALSE.
#'
#' @returns A numeric value indicating the total energy.
#' @export
cal_block_energy_with_iia <- function(block, char_list, weights, fun_list, rater_chars,
                                      iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                                      verbose = FALSE) {
  
  energy <- 0
  n_chars <- length(char_list)
  
  # 1. Base Item Characteristics Energy 
  for (i in seq_len(n_chars)) {
    if (weights[i] == 0) next 
    
    char_vec <- char_list[[i]]
    func <- fun_list[[i]]
    
    block_vals <- apply(block, 1, function(row_idx) func(char_vec[row_idx]))
    energy <- energy + sum(block_vals) * weights[i]
  }
  
  # 2. Inter-Item Agreement (IIA) Energy 
  do_bplin  <- iia_weights[1] != 0
  do_bpquad <- iia_weights[2] != 0
  do_aclin  <- iia_weights[3] != 0
  do_acquad <- iia_weights[4] != 0
  
  if (do_bplin || do_bpquad || do_aclin || do_acquad) {
    iia_energy <- 0
    
    for (row in 1:nrow(block)) {
      selected_item <- rater_chars[, block[row, ], drop = FALSE]
      
      bplin_val <- 0; bpquad_val <- 0; aclin_val <- 0; acquad_val <- 0
      
      if (do_bplin) {
        bplin_val <- bp.coeff.raw(selected_item, weights = "linear")$est[, 4]
        iia_energy <- iia_energy + (iia_weights[1] * bplin_val)
      }
      if (do_bpquad) {
        bpquad_val <- bp.coeff.raw(selected_item, weights = "quadratic")$est[, 4]
        iia_energy <- iia_energy + (iia_weights[2] * bpquad_val)
      }
      if (do_aclin) {
        aclin_val <- gwet.ac1.raw(selected_item, weights = "linear")$est[, 4]
        iia_energy <- iia_energy + (iia_weights[3] * aclin_val)
      }
      if (do_acquad) {
        acquad_val <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[, 4]
        iia_energy <- iia_energy + (iia_weights[4] * acquad_val)
      }
      
      if (verbose) {
        cat(sprintf("Block %d -> BPlin: %f, BPquad: %f, AClin: %f, ACquad: %f\n", 
                    row, bplin_val, bpquad_val, aclin_val, acquad_val))
      }
    }
    energy <- energy + iia_energy
  }
  
  return(energy)
}