#' unnormalize a matrix
#'
#' unnormalize returns a matrix normalized to the [0-1] basis back to its unnormalized state,
#'  given reference vectors of mins and maxes.
#'
#' @param X_n matrix on [0-1] basis
#' @param un_mins vecror of mins of unnormalized matrix, elements corresponding to columns of X_n
#' @param un_maxes
#'
#' @return unnormalized matrix
#' @export
#'
#' @examples unnormalize(X_n, un_mins, un_maxes)
unnormalize <- function(X_n, un_mins, un_maxes){
  # Return a normalized matrix to it's
  # un-normalized state, given a vector of
  # mins and a vector of maxes.

  un <- sweep(X_n, 2, (un_maxes - un_mins), FUN = '*')
  unnormed <- sweep(un, 2, un_mins, FUN ="+")
  unnormed

}
