#' Create a one-at-a-time perturbation design.
#'
#' @param X Design matrix, e.g. latin hypercube.
#' @param n Number of required design points in each dimension.
#' @param med Use median rather than mean?
#' @param hold Points at which to hold inputs, rather than mean or median.
#'
#' @return A matrix of design points.
#' @export
#'
#' @examples oaat_design(X, 21)
oaat_design <- function(X, n, med = TRUE, hold = NULL){
  # function for creating one-at-a-time design matrix
  # INPUTS:
  # X .... original design (e.g. a latin hypercube or output from expand.grid)
  # n ......... number of design points in each dimension
  # med ....... Use median rather than mean?
  # hold ...... Use supplied value to hold the non-changing points
  #
  # OUTPUTS:
  # ........... (n x nd) rows, nd columns design matrix, sweeping through parameter space

  oamat <- NULL

  nd <- ncol(X)

  if(med){
    meandes <- apply(X,2,median)
  }

  else{
    meandes <- apply(X,2,mean)
  }

  if(is.null(hold) == FALSE){

    meandes <- hold
  }

  mindes <- apply(X,2,min)
  maxdes <- apply(X,2,max)

  for (j in 1:nd){
    # base matrix of 'best' values to put the sweep values into
    basemat <- matrix(meandes, nrow = n, ncol = nd , byrow = TRUE)
    # use seq and length.out
    vec <- seq(from = mindes[j], to = maxdes[j], length.out = n)
    basemat[ ,j] <- vec
    oamat <- rbind(oamat,basemat)

  }

  oamat
}
