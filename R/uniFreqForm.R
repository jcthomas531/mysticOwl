#' uniFreqForm - Formatting Univariate Frequency Tables
#' @param x an output of a frequency table from uniFreq() or unifreq2()
#' @param varName the desired name of the variable as a string
#' @param extraText extra text to add into the title
#' @return a formatted univariate frequency table
#' @details
#'    function for formatting the frequency tables caclulated with uniFreq and uniFreq2 functions.
#'    with uniFreq: pass into x the item from the list to be formatted, use double bracket subsetting. this is done for one single element of uniFreq output at a time
#'    can also call names on the uniFreq object to know which number to use in bracket
#'    for unifreq: to display all of the tables you want, use a for loop in r markdown but make sure that you have results='asis' in the header otherwise it prints a bunch of crap
#' @examples 
#' #test
#' @import kableExtra
#' @export
uniFreqForm <- function(x, varName = NULL, extraText = NULL) {
  #default variable name to the var name in the data
  if (is.null(varName)) {
    varName <- colnames(x)[1]
  }
  
  formed <- x |>
    kable(caption = paste("Frequencies for", varName, "variable,", extraText), 
          longtable = FALSE,
          format = "latex",
          booktabs=TRUE,
          col.names = c(varName, "Freq.", "Cum. Freq.", 
                        "Percent", "Cum. Percent"),
          linesep = "",
          align = c("l", "r", "r", "r", "r", "r")) |>
    kable_styling(latex_options = c("HOLD_position", "scale_down")) |> 
    column_spec(1,bold=TRUE) |>
    row_spec(0, bold = TRUE) 
  return(formed)
}