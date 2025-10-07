#' uniFreqForm - Formatting Univariate Frequency Tables
#' @param x an output of a frequency table from uniFreq() or unifreq2()
#' @param varName the desired name of the variable as a string, default is NULL
#'  and defaults to name of the variable in the table. The supplied name or the
#'   name in the table cannot contain an underscore (probably other special 
#'   characters too).
#' @param extraText extra text to add into the title
#' @return a formatted univariate frequency table
#' @details
#'    Function for formatting the frequency tables caclulated with uniFreq 
#'    and uniFreq2 functions. Formats to latex for outputting in R markdown pdf docs \n
#'    This function operates on a single table at a time. That means if you have
#'     used uniFreq and have a list of output tables, you need to loop/lapply 
#'     through them and apply this formatting function to one table at a time. \n
#'    If you have a list of uniFreqForm outputs and want to have them all output 
#'    in an R markdown, make sure the chunk has results="asis" in the chunk header.
#' @examples 
#' #ouput examples
#' #say you have a list of tables formatted with uniFreqForm called univars
#' #all of the following work to output nice looking tables to a Rmd pdf
#' 
#' lapply(univars, function(x) {cat(x, sep = "\n")})
#' 
#' univars[1:length(univars)]
#' 
#' for (i in seq_along(univars)) {
#'   cat(univars[[i]], sep = "\n")
#' }
#' 
#' @import kableExtra
#' @export
uniFreqForm <- function(x, varName = NULL, extraText = NULL) {
  #default variable name to the var name in the data
  if (is.null(varName)) {
    varName <- colnames(x)[1]
  }
  #warning for special characters in variable name
  if (grepl("_", varName)) {
    
    warning(paste0("WARNING: for variable ", varName, ". Cannot have _ in a 
                   variable name. Rmarkdown will get mad. Probably other special
                   characters off limits too. The underscore has been replaced by a space :)"))
    varName <- gsub("_", " ", varName)
  }
  #formatting
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