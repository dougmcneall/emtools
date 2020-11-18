#' A matrix of samples from normal distributions
#'
#' @param n number of desired samples.
#' @param means vector of desired means, one per column.
#' @param sds vector of desired standard deviations, one per column.
#'
#' @return matrix containing samples from normal distributions in the columns.
#' @export
#'
#' @examples samp_unif(20, means = c(1,10), sds = c(1,10))
samp_norm <- function(n, means, sds){
  # Sample from a normal distribution, and place in
  # a m = length(mins or maxes) x n matrix.
  
  out <- rnorm(n=length(means)*n, mean=means , sd = sds)
  dim(out) <- c(length(means),n)
  rownames(out) <- names(means)
  return(t(out))
}
