#' uniFreq2 - Frequency Table for a Single Variable
#' @param data the name of the data frame
#' @param variable the name of the variable stored as a character
#' @param includeNA whether or not to include missing values in the frequency table, default is \code{TRUE}
#' @return a univariate frequency table
#' @details
#'    create a frequency table for a single variable at a time.
#'    can be formatted with the freqFormat function.
#'    a potential issue here is if you use this with a numeric variable it will probably have all the factor levels in a random order.
#'    to combat this, could add an if statement to this code to specifically handle turning numeric to factor and getting the order of the levels right.
#' @examples 
#' #test
#' @import dplyr
#' @export
uniFreq2 <- function(data, variable, includeNA = TRUE) {
  if (include_NA == TRUE) {
    #select column of interest and create basic summary
    sumDat <- data |>
      select(all_of(variable)) |>
      mutate(across(all_of(variable), ~ as.factor(.)),
             across(all_of(variable), ~ addNA(.))) |>
      group_by(across(all_of(variable)), .drop = FALSE) |>
      summarise("Freq" = n(),
                "freqPercent" = n()/nrow(data))
    #cumulative frequencies and percents
    sumDat$cumFreq <- cumsum(sumDat$Freq)
    sumDat$cumPercent <- cumsum(sumDat$freqPercent)
    #formatting
    sumDat <- sumDat |>
      relocate(cumFreq, .after = Freq) |>
      mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"),
             cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
             Freq = format(Freq, big.mark = ",", scientific = FALSE),
             cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE))
  } else { #no including NA
    sumDat <- data |>
      select(all_of(variable)) |>
      mutate(across(all_of(variable), ~ as.factor(.))) |>
      group_by(across(all_of(variable)), .drop = FALSE) |>
      summarise("Freq" = n(),
                "freqPercent" = n()/nrow(data))
    sumDat$cumFreq <- cumsum(sumDat$Freq)
    sumDat$cumPercent <- cumsum(sumDat$freqPercent)
    sumDat <- sumDat |>
      relocate(cumFreq, .after = Freq) |>
      mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"),
             cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
             Freq = format(Freq, big.mark = ",", scientific = FALSE),
             cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE))
  }
  return(sumDat)
}
