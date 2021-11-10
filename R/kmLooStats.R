
#' Simple Leave-one-out statistics for a km (DiceKriging) model
#'
#' @param km model fit as a km (DiceKriging) object
#' @param type Typr of kriging, one of 'SK' (simple kriging), or 'UK' (universal kriging)
#'
#' @return A list of statistics summarising leave-one-out prediction performance. loo is the prediction, mae is Mean Absolute Error, 
#' maxerr is the maximum error and pmae is the mean absolute error expressed as a proportion of the range of the output.
#' @export
#'
#' @examples
kmLooStats <- function(km, type = 'UK'){
  
  loo <- leaveOneOut.km(km, type = type, trend.reestim = TRUE)
  
  preddiff <- loo$mean - km@y
  
  mae <- mean(abs(preddiff))
  rmse <- sqrt(mean(preddiff^2)) 
  maxerr <- max(loo$mean)
  
  absdiff <- abs(diff(range(km@y)))
  pmae <- (mae / absdiff) * 100
  
  return(list(loo = loo, mae = mae, pmae = pmae, maxerr = maxerr))
  
}