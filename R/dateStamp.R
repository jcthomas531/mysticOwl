#' dateStamp - create a string with nicely formatted date/time
#' @details
#' I designed this to be an easy way to put time stamps on output files so they can be easily tracked. The format is designed so that when files are sorted alphabetically, the files will order themselves chronolocially. Running the function without any arguements returns a date and time stamp in the fromat yyyy_mm_dd_hh_mm. Adding the text arguement appends a string to the end of the date and time stamp. The option includeTime lets you toggle whether you want to time included in the stamp, allows you to decide how granular you want the output to be.
#' @param text a string to append to the end of the date/time stamp. Defaults to nothing. Idea is to use for file names.
#' @param includeTime logical, whether the time should be included in the stamp. Defaults to TRUE.
#' @return a string with the date/time stamp
#' @examples 
#' dateStamp()
#' dateStamp(text = "testFile.txt", includeTime = TRUE)
#' dateStamp(text = "testFile.txt", includeTime = FALSE)
#' @import stringr
#' @export



dateStamp <- function(text = "", includeTime = TRUE) {
  stamp <- Sys.time() |>
    stringr::str_replace_all("[- :]", "_") |>
    stringr::str_remove("_[^_]*$") 
  
  if (includeTime == FALSE) {
    stamp <- stamp |> stringr::str_remove("_[0-9]{2}_[0-9]{2}$")
  }
  
  return(paste0(stamp, text))
}
