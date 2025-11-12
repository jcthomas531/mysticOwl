#' lagWindow - counting number of elements in moving window
#' @details
#'    this is designed to just be a helper function for lagCountSurvival(). I know that there is a way to make it so this function is not available to the user and only available to code within this package but i dont want to do that right now. i believe all you need to do is remove the export command but im cool with this for now
#' @param x the variable to work with
#' @param windowWidth size of the window
#' @return a vector of counts in a window
#' @examples 
#' #see lagCountSurvival()
#' @export


lagWindow <- function(x, windowWidth = 365) {
  #extract information
  vecLength <- length(x)
  #count of elements within moving window
  retVec <- rep(NA, vecLength)
  for (i in 1:vecLength) {
    retVec[i] <- sum(
      #only need to count the elements previous to current element
      #thats why the LHS vector is subset
      (x[1:i] >= (x[i] - windowWidth)) & (x[1:i] < x[i])
    )
  }
  return(retVec)
}