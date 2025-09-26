#' percForm - Formatting Percentages as Strings
#' @param x  values or values taking value 0-100 to be formatted
#' @return a string formatted as a percentage with the percentage symbol
#' @details
#'    Formatting percentages as strings with 2 decimal places and the percentage symbol. This is nice when making tables and some of my univariate table functions use this function.
#' @examples 
#' #test
#' @export
percForm <- function(x){
  return(paste0(round(x,2), "%"))
}