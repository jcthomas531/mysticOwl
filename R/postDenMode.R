#' postDenMode - calculate posterior density mode from posterior samples
#' @details
#' Calculate posterior density mode from posterior samples, this is often perfered over the posterior mean but in case of symmetry it will be about the same. For continuous variables, this translates to the value associated with the max density in the posterior. This has an if clause in it for when all the posterior draws are the same ie when you are doing some sort of imputation task and the value does not need to be estimated bc it is known.
#' @param pd a vector of posterior draws
#' @return the posterior density mode
#' @examples 
#' #placeholder
#' @export

postDenMode <- function(pd) {
  if (length(unique(pd)) != 1) {
    d <- density(pd)
    return(d$x[which.max(d$y)])
  } else if (length(unique(pd)) == 1) {
    message("all draws eqaul, return the mean/median as posterior density mode may not be the singular drawn value")
    return(mean(pd))
  }
}