#' gtToPdf - gtsummary table to pdf output
#' @details
#' Take the output of a gtsummary and make it knit nicely to pdf in a way that looks like the other tables I make. The formatting here isnt perfect and could use some work with centering and size and title and such but that should be easier now that it is in a kable format. gtsummary is my enemy because of how hard it is to knit to pdf and have it look nice but they do make a dang good table. This function helps bridge the gap. The table doesnt look that great in the viewer pane but it does look nice when knit to a pdf.
#' @param x a gt summary table
#' @return A kable style table that can be knit in a pdf
#' @examples 
#' #simple proof of concept with iris data
#' iris |> 
#' gtsummary::tbl_summary(
#'   include = c(Sepal.Length),
#'   by = Species) |>
#'   gtToPdf()
#' @importFrom gtsummary modify_header
#' @importFrom gtsummary bold_labels
#' @importFrom gtsummary as_kable_extra
#' @export
gtToPdf <- function(x) {
  x |>
    gtsummary::modify_header(
      label ~ ""
    ) |>
    gtsummary::bold_labels() |>
    gtsummary::as_kable_extra(booktabs = TRUE)
}


