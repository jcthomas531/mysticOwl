#' prop2SampN - Sample size calculation for 2 sample proportion test
#' @details
#' This test is for two sided tests only, with n being the sample size for each group and assuming equal allocation. For details on the calculations, see clinical trials notes.
#' @param p1 assumed proportion in group 1
#' @param p2 assumed proportion in group 2
#' @param alpha_ desired alpha level
#' @param power desired power
#' @return a list containing 
#' \code{conditionalN} the sample size needed to detect specified difference at desired power level with desired alpha for the conditional test
#' \code{unconditionalN} the sample size needed to detect specified difference at desired power level with desired alpha for the unconditional test 
#' @examples
#' prop2SampN(p1 = 0.16, p2 = 0.10, alpha_ = 0.05, power = .85)
#' @export

prop2SampN <- function (p1, p2, alpha_, power) {
  #z stats
  zAlpha2 <- qnorm(1 - alpha_/2) #z_{1-alpha/2}
  zBeta <- qnorm(power) #z_{1-beta}
  #p bar for conditional test
  pBar <- (p1+p2)/2
  
  
  #for conditional test
  conNum1 <- zAlpha2 * sqrt(2*pBar*(1-pBar))
  conNum2 <- zBeta * sqrt(p1*(1-p1) + p2*(1-p2))
  conNum <- (conNum1 + conNum2)^2
  conDen <- (p1-p2)^2
  #n calculation
  nCon <- conNum/conDen
  
  
  #for unconditional test
  uncon1 <- (zAlpha2 + zBeta)^2
  uncon2 <- (p1 - p2)^2
  uncon3 <- p1*(1-p1) + p2*(1-p2)
  #n calculation
  nUncon <- (uncon1/uncon2)*uncon3
  
  
  return(list("conditionalN" = nCon,
              "unconditionalN" = nUncon))
  
}
prop2SampN(p1 = 0.16, p2 = 0.1072, alpha_ = 0.05, power = .85)
