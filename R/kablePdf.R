#' kablePdf - Formatting a Table for Markdown PDF
#' @param table  the table to format, can be piped
#' @param title a title for the table, default is "title"
#' @param colNames a vector of strings containing new names for the columns
#' @param digits the number of digits to display
#' @return a formatted table
#' \item{retTab}{a formatted table}
#' @details
#'    Basic table formatting for a markdown pdf document. This gets things set up
#'    in a standard way that I like, but can be further adjusted by piping in to 
#'    further kableExtra functions. I havent full explored all of the options, but
#'    you can at least do some things. See examples
#' @examples 
#' #piping custom table into further kableExtra functions for further customization
#' head(iris) |>
#'   kablePdf(title = "Iris Example") |>
#'   #change column 3 justificiation to left
#'   column_spec(3, latex_column_spec = "l")
#' @import kableExtra
#' @export
kablePdf <- function(table, 
                          title = "title",
                          colNames = NULL,
                          digits = NULL) {
  #argument set up
  kableArgs <- list(x = table, caption = title,
                    longtable=FALSE, format = "latex", booktabs = TRUE)
  if (!is.null(colNames)) kableArgs$col.names <- colNames
  if (!is.null(digits)) kableArgs$digits = digits
  #table creation
  retTab <- do.call(kable, kableArgs) |>
    kable_styling(latex_options = c("HOLD_position", "scale_down")) |>
    column_spec(1,bold=TRUE) |>
    row_spec(0, bold = TRUE) 
  
  
  #since this returns a kable object, you can pipe it into other kable extra funs
  return(retTab)
  
}
