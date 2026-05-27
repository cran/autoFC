#' Construction of Random Item Blocks
#'
#' @description Generates a matrix of randomly paired blocks, where each row 
#' represents a block of items. This serves as a fast, unstructured initial 
#' solution for forced-choice test assembly algorithms.
#'
#' @param total_items Integer. The total number of items available in the item pool.
#' @param target_items Integer. The number of items to sample from \code{total_items} 
#'        to build the blocks. Defaults to \code{total_items}. Must be \eqn{\le} \code{total_items}.
#' @param block_size Integer. The number of items in each block (e.g., 2 for pairs, 
#'        3 for triplets). Must be \eqn{\ge 2}.
#'        
#' @details Given the total number of items in the pool, this function randomly samples 
#' \code{target_items} and formats them into a block matrix. 
#' 
#' If \code{target_items} is not a multiple of \code{block_size}, the function will 
#' randomly draw additional items from the sampled pool to fill the incomplete final block, 
#' ensuring the matrix structure is strictly maintained.
#'
#' @returns A matrix of integers indicating the item IDs. The number of rows equals 
#'          \code{ceiling(target_items / block_size)}, and the number of columns 
#'          equals \code{block_size}.
#'
#' @examples
#' # Use all 60 items to build 20 blocks of 3
#' make_random_block(total_items = 60, block_size = 3)
#'
#' # Sample 45 items from a pool of 60 to build 15 blocks of 3
#' make_random_block(total_items = 60, target_items = 45, block_size = 3)
#'
#' # Handle cases where target_items is not a multiple of block_size
#' # (Will randomly reuse 1 item to fill the last triplet)
#' make_random_block(total_items = 60, target_items = 50, block_size = 3)
#'
#' @export
make_random_block <- function(total_items, target_items = total_items, block_size) {
  
  # 1. Clean and concise input validation
  if (total_items < 1 || total_items %% 1 != 0) stop("'total_items' must be a positive integer.")
  if (target_items < 1 || target_items %% 1 != 0) stop("'target_items' must be a positive integer.")
  if (block_size < 2 || block_size %% 1 != 0) stop("'block_size' must be an integer >= 2.")
  
  if (target_items > total_items) stop("'target_items' cannot be greater than 'total_items'.")
  if (block_size > target_items) stop("'block_size' cannot be greater than 'target_items'.")
  
  # 2. Sample the target items from the pool
  item_indices <- sample.int(total_items, size = target_items, replace = FALSE)
  
  # 3. Check if target is a perfect multiple of block size
  remainder <- target_items %% block_size
  
  if (remainder == 0) {
    # Perfect fit
    return(matrix(item_indices, ncol = block_size, byrow = TRUE))
    
  } else {
    # Not a multiple: How many extra items do we need to fill the last block?
    items_needed <- block_size - remainder
    
    # Randomly select 'items_needed' from the already sampled pool to fill the gap.
    # (Random selection prevents systematic over-representation of item 1 & 2).
    filler_items <- sample(item_indices, size = items_needed, replace = FALSE)
    
    # Combine and matrix
    item_indices <- c(item_indices, filler_items)
    return(matrix(item_indices, ncol = block_size, byrow = TRUE))
  }
}