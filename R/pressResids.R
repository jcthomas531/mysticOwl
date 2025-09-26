#' pressResids - Calculate PRESS residuals
#' @param model  an lm model
#' @return A list containing values:
#' \code{pressRes} the press residuals
#' @details
#'    Function for calculating the press residuals for a lm model.
#'    There are existing versions of this in some packages but most just give you the press statistic rather than the resids and the one i found that did give resids was not very cooperative. 
#'    This one works and is simple. It is only designed to work with lm and accepts an lm object as the sole arguement and outputs the press residuals aka the predicted residuals.
#'    Shout out to Dr. Zamba and linear models hw9 for this simple approach to calculation.
#'    I checked the results of this calculation against that of an existing package and they are correct.
#' @examples 
#' #test
#' @export
pressResids <- function(model) {
  #extract model matrix
  X <- model.matrix(model)
  #calculate projection matrix and extract diagonals
  pii <- (X %*% solve(t(X) %*% X) %*% t(X)) |>
    diag()
  #calculate regular residuals
  res <- resid(model)
  #calculate press residuals
  pressRes <- res/(1-pii)
  
  return(pressRes)
}
