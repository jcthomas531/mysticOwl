#' binomScoreTest - binomial score test
#' @param y  test
#' @return A list containing values:
#' \item{ML_estimate}{test1}
#' \item{test_stat}{test2}
#' \item{p_val}
#' \item{ci}
#' @details
#'    some explaination
#' @examples 
#' #test
#' @export
binomScoreTest <- function (y, n, pi_hy = .5, alpha = .05) {
  #maximum likelihood estimate of pi
  pi_ml <- y/n
  #score test statistic, see page 13 of agresti's categorical data book
  z <- (pi_ml - pi_hy)/sqrt(pi_hy*(1-pi_hy)/n)
  #p-value for score test statistic
  pVal <- pnorm(abs(z), lower.tail = FALSE)*2
  #confidence interval, see page 14 of agresti's categorical data book
  #endpoints found via optimization, uniroot function
  lowEnd <- function (x) {
    (pi_ml - x)/sqrt(x*(1-x)/n) - (qnorm(alpha/2, lower.tail = FALSE))
  }
  highEnd <- function (x) {
    (pi_ml - x)/sqrt(x*(1-x)/n) + (qnorm(alpha/2, lower.tail = FALSE))
  }
  ciLow <- uniroot(lowEnd, interval = c(0,1))[[1]]
  ciHigh <- uniroot(highEnd, interval = c(0,1))[[1]]
  
  
  return(list(ML_estimate = pi_ml,
              test_stat = z,
              p_val = pVal,
              ci = c(ciLow, ciHigh)
              )
         )
  
}


