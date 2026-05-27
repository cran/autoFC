#' Function for Checking If All Items in a Vector Are Unique
#'
#' @description Returns \emph{1} if each element in the vector is unique,
#' and \emph{0} otherwise.
#'
#' @usage facfun(vec)
#'
#'
#' @param vec Input vector.
#'
#' @returns \emph{1} if each element in the vector is unique,
#' and \emph{0} otherwise.
#'
#' @author Mengtong Li
#'
#' @examples
#'   facfun(c("Openness", "Neuroticism", "Agreeableness"))
#'   facfun(c("Openness", "Openness", "Agreeableness"))
#' @export

facfun <- function(vec) {
  
  if (length(vec) == 0) stop("'vec' must contain at least one element.")
  
  # as.integer() instantly converts TRUE to 1 and FALSE to 0.
  # anyDuplicated() is a highly optimized C-level function in R that is 
  # significantly faster than length(unique(vec)).
  
  return(as.integer(anyDuplicated(vec) == 0L))
}