#' Fast Calculation of Item Block "Energy"
#'
#' @description Calculates the total "energy" of one or multiple paired item blocks.
#' This function has been heavily optimized for speed, as it is called thousands 
#' of times during the Simulated Annealing optimization loop.
#'
#' @param block An \emph{n} by \emph{k} integer matrix of item indices.
#' @param char_list A list of vectors, where each vector contains the full item pool 
#'        data for one specific characteristic.
#' @param weights A numeric vector of weights.
#' @param fun_list A list of pre-matched R function objects (e.g., `list(facfun, var)`).
#'
#' @returns A numeric value indicating the total energy for the given item blocks.
#' @export
cal_block_energy <- function(block, char_list, weights, fun_list) {
  
  energy <- 0
  n_chars <- length(char_list)
  
  for (i in seq_len(n_chars)) {
    if (weights[i] == 0) next # Skip traits that have zero weight
    
    char_vec <- char_list[[i]]
    func <- fun_list[[i]]
    
    block_vals <- apply(block, 1, function(row_idx) func(char_vec[row_idx]))
    energy <- energy + sum(block_vals) * weights[i]
  }
  
  return(energy)
}