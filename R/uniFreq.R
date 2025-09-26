#' uniFreq - Frequency Table for all Variables
#' @param x  a data.frame where each row represents an observation
#' @return a list of univariate frequency tables, one for each variable in the data set
#' @details
#'    This function creates a univariate frequency table for each variable in the data set. I generally tend to use uniFreq2 more since this does all variables and that sometimes is a little much.
#' @examples 
#' #test
#' @importFrom arsenal freqlist
#' @export
uniFreq <- function(x) {
  #preparing each frequency table
  freqLookup <- 1:ncol(x)
  freqHolder <- vector(mode = "list", length = length(freqLookup))
  for (i in 1:length(freqHolder)) {
    #get the appropriate column number
    colnum <- freqLookup[i]
    #create frequency table
    freqHolder[[i]] <- x[,colnum] |> 
      table() |>
      arsenal::freqlist() |>
      as.data.frame()
    #format percentages correctly
    freqHolder[[i]]$freqPercent <- percForm(freqHolder[[i]]$freqPercent)
    freqHolder[[i]]$cumPercent <- percForm(freqHolder[[i]]$cumPercent)
    #rename first column with variable name
    colnames(freqHolder[[i]])[1] <- colnames(x)[colnum]
    #rename list item with correct variable name
    names(freqHolder)[i] <- colnames(x)[colnum]
  }
  return(freqHolder)
}