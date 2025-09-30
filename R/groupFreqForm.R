#' groupFreqForm - Formatting Grouped Frequency Tables
#' @param x an output of a frequency table from groupFreq()
#' @param varNames a vector of strings that are the desired presentation names of the variables 
#' @param extraText any additional things you want to add to title
#' @return a formatted grouped frequency table
#' @details
#'    formatting function for groupFreq()
#'    THIS SHOULD PROBABLY BE UPDATED
#' @examples 
#' #test
#' @import kableExtra
#' @export
groupFreqForm <- function(x, varNames = NULL, extraText = NULL) {
  #number of variables in the table
  varNum <- ncol(x) - 5 #if you add more columns, this must change
  #default variable name to the var name in the data
  if (is.null(varNames)) {
    varNames <- colnames(x)[1:varNum]
  }
  
  
  #interval for the highlight switching
  #total number of rows divided by number of unique categories should give the desired intervals
  groupSize <- (x[,1] |> nrow())/(x[,1] |> unique() |> nrow())
  rows_to_color <- which((1:nrow(x) - 1) %/% groupSize %% 2 == 0)
  
  formed <- x |>
    kable(booktabs=TRUE,
          longtable = TRUE,
          #"latex",
          caption = paste("Frequencies for", varNames[1], "variable, by",
                          paste(varNames[2:varNum], extraText, sep = ", ")), 
          col.names = c(varNames, "Freq.", "Cum. Freq.", 
                        "Percent", "Cum. Percent", "Total Cum. Freq"),
          linesep = "",
          align = c("l", "l", "r", "r", "r", "r", "r")) |>
    kable_styling(latex_options = c("HOLD_position", "repeat_header")) |> 
    column_spec(1:varNum,bold=TRUE) |>
    row_spec(0, bold = TRUE) |>
    row_spec(rows_to_color, background = "#F0F0F0")
  return(formed)
}
