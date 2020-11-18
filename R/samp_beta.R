#' A matrix of samples from beta distributions
#'
#' @param n Desired number of samples.
#' @param mins vector containing minima of samples in original space.
#' @param maxes vector containing maxima of sample in original space.
#' @param wrt a design that samples should be normalized to.
#' @param shape1 shape parameter for the beta distribution.
#' @param shape2 shape parameter for the beta distribution.
#'
#' @return list containing 
#' \itemize{
#'   \item x.unn - matrix of beta distributions in the space defined by mins, maxes
#'   \item x.nwrt - matrix of beta distributions normalized to the space desribed by wrt
#' }
#' @export
#'
#' @examples
samp_beta <- function(n, mins, maxes, wrt, shape1 = 2, shape2 = 2){
  # n is desired number or samples
  # mins, maxes are desired limits of samples in original space
  # wrt is : what should samples be normalized to (typically design)
  # shapes are shape parameters for the beta dist.
  # OUTPUTS
  # x.unn is unnormalized beta sample (defined by mins, maxes)
  # x.nwrt is normalized with respect to matrix wrt
  
  n.beta <- rbeta(n = length(mins)*n, shape1 = shape1,shape2 = shape2)
  dim(n.beta) <- c(length(mins),n)
  rownames(n.beta) <- names(mins)
  n.beta <- (t(n.beta))
  
  
  # beta distributions in the space defined by mins, maxes
  x.unn <- unnormalize(n.beta,mins,maxes)
  
  # Then, normalized to the space desribed by wrt
  x.nwrt <- n.wrt(x.unn,wrt)
  
  return(list(x.unn = x.unn,x.nwrt = x.nwrt))
  
}