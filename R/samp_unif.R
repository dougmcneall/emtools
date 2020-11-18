#' A matrix of samples from a uniform distribution
#'
#' @param n number of rows
#' @param mins desired column minima
#' @param maxes desirde column maxima
#'
#' @return A matrix of samples from a uniform distribution
#' @export
#'
#' @examples samp_unif(20, mins = c(0,0), maxes = c(1,1))
samp_unif <- function(n, mins, maxes){
  # Sample from a uniform distribution, and place in
  # a m = length(mins or maxes) x n matrix.
  out <- runif(n=length(mins)*n, min=mins , max = maxes)
  dim(out) <- c(length(mins),n)
  rownames(out) <- names(mins)
  return(t(out))
}
