#' probDirect - probability of direction for posterior samples
#' @details
#' What is the probability that the parameter is positive/negative? The sign is based on the posterior density mode, ie if the posterior density mode is positive, this is the probability that the parameter is positive.
#' @param pd a vector of posterior draws
#' @return the probability of direction
#' @examples 
#' #placeholder
#' @export

probDirect <- function(pd) {
  #based on posterior density mode
  pdMode <- postDenMode(pd)
  if (pdMode < 0) {
    return(mean(pd < 0))
  } else if (pdMode >= 0) {
    return(mean(pd >= 0))
  }
}