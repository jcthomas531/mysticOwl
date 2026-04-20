#' gkGammaTest - Goodman Kruskal Gamma Test
#' @details
#' This function is simply an extension of the Goodman and Kruskal Gamma test function vcdExtra::GKgamma() that also outputs a p-value. For details on this method see categorical data analysis by agresti (v3), page 88. The standard error (or ASE) from the vcdExtra function was checked against SAS and is correct.
#' @param x a contingency table
#' @param ... other arguements to be passed into vcdExtra::GKgamma() such as level. See documentation for vcdExtra::GKgamma() for more details.
#' @return A list containing values:
#' \code{vcdOutput} the output from the vcdExtra::GKgamma() function
#' \code{zStat} the z statistic
#' \code{pVal} the p-value
#' @examples 
#' #simple proof of concept with mtcars data
#' carsTab <- table(mtcars$carb, mtcars$vs)
#' gkGammaTest(carsTab)
#' @importFrom vcdExtra GKgamma
#' @export
gkGammaTest <- function (x, ...) {
  #will take a contigency table, just like vcdExtra::GKgamma()
  
  #use the vcdExtra::GKgamma() function
  vcdRes <- vcdExtra::GKgamma(x, ...)
  
  #use results of function to calculate p-value
  zStat <- vcdRes$gamma/vcdRes$sigma
  pVal <- pnorm(abs(zStat), lower.tail = FALSE)*2
  
  #results as a list
  res <- list("vcdOutput" = vcdRes,
              "zStat" = zStat,
              "pVal" = pVal)
  return(res)
}
