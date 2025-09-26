#' easyColors - Pallet of N Colors
#' @param n  number of colors to return
#' @return A list containing values:
#' \code{resultHolder} placeholder
#' @details
#'    I believe this is how ggplot chooses colors for discrete variables, it is \code{n} equally spaced colors based on rgb value
#' @examples 
#' #test
#' @export
easyColors <- function(n) {
  #I believe this is how ggplot chooses colors for discrete variables
  #takes n, the number of colors you want
  #stolen from stack overflow
  #https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}