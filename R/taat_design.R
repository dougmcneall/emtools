#' Two-at-a-time (taat) parameter perturbation design
#'
#' @param X Design matrix on which to base the taat design.
#' @param n number of samples per dimension.
#' @param means values at which to hold the non-perturbed parameters.
#'
#' @return matrix containing two-at-a-time perturbation design. Each block of rows perturbs two parameters, with all other parameters held at mean, or supplied values.
#' @export
#'
#' @examples taat_design(X, n = 21)
taat_design <- function(X, n, means = NULL){
  # Build a two at a time emulator design
  # hold all of the other parameters at their mid values
  
  maxes <- apply(X, 2, max)
  mins  <- apply(X, 2, min)
  
  if(is.null(means)) means <- apply(X, 2, mean)
  
  nip <- ncol(X) # number of input parameters
  
  col.ix <- combn(1:nip,2)
  
  em.vec <- seq(from = 0, to = 1, length.out = n)
  
  des.cols <- expand.grid(em.vec, em.vec)
  
  holder <- matrix(means, ncol = nip, nrow = nrow(des.cols), byrow = TRUE)
  
  out <- NULL
  
  for(i in 1:ncol(col.ix)){
    
    mat.part <- holder
    
    colu <- col.ix[,i]
    
    mat.part[, as.matrix(colu[1])] <- des.cols[,1]
    mat.part[, as.matrix(colu[2])] <- des.cols[,2]
    
    out <- rbind(out, mat.part)
    
  }
  
  return(list(des = out, ix = col.ix))
  
}
