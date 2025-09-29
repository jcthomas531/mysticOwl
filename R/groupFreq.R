#' groupFreq - Frequency Table for Multiple Variables
#' @param data the data set
#' @param variable a string with the name of the main variable of interest
#' @param groupings a vector of strings with the name of the variables interested in grouping on
#' @param includeNA whether or not to include missing values in the frequency table, default is \code{TRUE}
#' @return a grouped frequency table
#' @details
#'    create a frequency table based on a grouping
#'    like uniFreq2(), this creates one table at a time
#'    this function has a different formatting function than uniFreq() and uniFreq2()
#'    THIS IS SET UP FOR ONLY TWO VARIABLES
#'    ie ONE VARIABLE FOR variable ARGUMENT, ONE VATIABLE FOR groupings ARGUEMENT
#'    IF YOU WANT TO UPDATE THIS FUNCTION TO TAKE MORE GROUPINGS YOU NEED TO ADD MORE UNGROUPING AND GROUPING ARGUEMENT
#'    TO GET THE cumulative frequency and cumulative percent correct
#' @examples 
#' #test
#' @import dplyr
#' @export
groupFreq <- function(data, variable, groupings, includeNA = TRUE) {
  #concatenate variables of intereste
  colsInter <- c(variable, groupings)
  #select columns of interest and create basic summary
  
  
  if (includeNA == TRUE) {
    sumDat <- data |>
      select(all_of(colsInter)) |>
      mutate(across(all_of(colsInter), ~ as.factor(.)),
             across(all_of(colsInter), ~ addNA(.))) |>
      group_by(across(all_of(colsInter)), .drop = FALSE) |>
      summarise("Freq" = n()
      ) |>
      ungroup() |>
      group_by(across(all_of(colsInter[1]))) |> 
      mutate(cumFreq = cumsum(Freq),
             freqPercent = Freq/sum(Freq),
             cumPercent = cumsum(freqPercent)
      ) #the logic to get this correct was insane lol
  } else { #excluding NA 
    sumDat <- data |> 
      select(all_of(colsInter)) |>
      na.omit() |>
      mutate(across(all_of(colsInter), ~ as.factor(.))) |>
      group_by(across(all_of(colsInter)), .drop = FALSE) |>
      summarise("Freq" = n()
      ) |>
      ungroup() |>
      group_by(across(all_of(colsInter[1]))) |> 
      mutate(cumFreq = cumsum(Freq),
             freqPercent = Freq/sum(Freq),
             cumPercent = cumsum(freqPercent))
  }
  
  
  #formatting
  sumDat$totCumFreq <- cumsum(sumDat$Freq)
  sumDat <- sumDat |>
    relocate(cumFreq, .after = Freq) |>
    mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"), 
           cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
           Freq = format(Freq, big.mark = ",", scientific = FALSE),
           cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE),
           totCumFreq = format(totCumFreq, big.mark = ",", scientific = FALSE)
    )
  
  
  
  return(sumDat)
}