#' lagCountSurvival - creating the start stop survival for the lag time varying covariate
#' @details
#' function for creating the intervals for the time varying covariate which is counts in a lag window, in this case number of recall appointments in 365 days this is set up for a particular analysis (isolation methods satoki) so the format may not be directly transferable to other data sets but with slight tweeks, it  can be made to fit, see the example data above. "recallTimes" is the amount of days after initial restoration that the patient had an appointment, stored as a character vector with specific format
#' @param x data formatted like in example
#' @param visitVar the name of the time varying variable in x to count (just the var name, no need to use a string)
#' @param failVar the name of the variable in x holding the failure information (just the var name, no need to use a string)
#' @param idVar the name of the variable in x that has ID information (just the var name, no need to use a string)
#' @param toothVar the name of the variable in x that has the tooth number information, in this use case, observations are uniquely identified thru id and tooth number but in other applications only one ID would be needed and this would have to be augmented (just the var name, no need to use a string)
#' @param windowWidth lag period to count in
#' @param splitChar the regular expression for the character that splits the visitVar
#' @return formatted start stop survival data with lag covariate
#' @examples 
#' datToy <- data.frame(id = c(1,1,2),
#'       tooth = c(4,5,4),
#'       fail = c(0,1,1),
#'       recallTimes = c("0|400",
#'                 "0|180|365|450|550|720|1460|1560",
#'                 "0|180|360|1440|1540"),
#'       sex = c("M", "M", "F"))
#' lagCountSurvival(x=datToy)
#' @import Hmisc
#' @import rlang
#' @import dplyr
#' @import stringr
#' @export




lagCountSurvival <- function(x, visitVar = recallTimes, failVar = fail,
                             idVar = id, toothVar = tooth, windowWidth = 365,
                             splitChar = "\\|") {
  #browser()
  #using enquosures to allow visitVar and failVar names to be set by user (diffusion)
  #this could be improved by enquos() the plural but this is fine for now
  visitVar <- enquo(visitVar)
  failVar <- enquo(failVar)
  idVar <- enquo(idVar)
  toothVar <- enquo(toothVar)
  #set up
  
  xNoFail <- x |>
    select(-!!failVar)
  failVec <- x[[as_name(failVar)]]
  
  #split out the recallTimes variable into a list
  #in the future, the regular expression to split here could be passed as an arg
  timeList <- str_split(xNoFail[[as_name(visitVar)]], "\\|")
  
  
  #get the lag counts for each id/tooth combo (ie original rows)
  origRows <- nrow(xNoFail)
  expandHolder <- vector(mode = "list", length = origRows)
  for (i in 1:origRows) {
    
    #make split times into data frame, lag the time for the end of the period
    periodDat <- timeList[[i]] |>
      as.data.frame() |>
      rename("time" = "timeList[[i]]") |>
      mutate(end = Hmisc::Lag(time, -1) |>
               as.numeric(),
             time = as.numeric(time)) |>
      slice(-n()) #remove last row
    
    #add back in the id and tooth number for this observation
    periodDat[[as_name(idVar)]] <- xNoFail[[as_name(idVar)]][i]   
    periodDat[[as_name(toothVar)]] <- xNoFail[[as_name(toothVar)]][i]
    
    #count of events in moving window
    #using "end" column as this is when events happened
    periodDat$windowCount <- lagWindow(periodDat$end, windowWidth = windowWidth)
    
    #incorperating the failure indicator
    #if the person failed, the indicator is on the last period, not all period
    #thus, this must be done differently than just rejoining to the orig dataset
    periodDatRow <- nrow(periodDat)
    periodDat[[as_name(failVar)]] <- c(rep(0, periodDatRow-1), failVec[i])
    
    #join to the rest of the demographic data 
    #as_name makes the enquo into a string, see also as_label()
    expandHolder[[i]] <- left_join(periodDat, xNoFail[i,],
                                   by = c(as_name(idVar), as_name(toothVar)),
                                   relationship = "many-to-one") |>
      select(-!!visitVar) 
    
  }
  
  #put the pieces together into single data set
  #this could be done with Matrix::rbindlist but i like this more
  return(do.call(rbind, expandHolder))
  
} 