#' summary.bss - summary function for "bss" objects
#' @details
#' summary for "bss" objects
#' @param object a bss object created by bestSubsetSel()
#' @param toReturn the name of the IC to return. Default is NULL and it returns all of the ICs. Options are "aic" and "bic" right now
#' @param n number of top models to return for each IC that is being returned
#' @return a table of the top models in order of lowest IC to highest IC. Each model formula is given as a string, not a formula, so you cant run something like "lm" on that columns contents. However, each model is given with its associated index from the bestSubsetSel() output so you can trace it back in that ouput.
#' @method summary bss
#' @examples 
#' data("iris")
#' 
#' lmBss <- bestSubsetSel(dat = iris, respVar = Sepal.Length,
#'  method = "lm", modelString = "+Sepal.Width")
#' summary(lmBss)
#' 
#' irisBin <- iris |> 
#' dplyr::mutate(Species = ifelse(Species == "virginica", 0, 1))
#' 
#' 
#' glmBss <- bestSubsetSel(dat = irisBin, respVar = Species,
#'  method = "glm", family = "binomial", modelString = "+Sepal.Width")
#' summary(glmBss)
#' @export



summary.bss <- function(object, toReturn = NULL, n = NULL) {
  #browser()
  #checking arguements assumptions
  if (
    !(
      isTRUE(toReturn %in% c("aic", "bic")) | #a bit of complex logic here to deal with the NULL
      is.null(toReturn)
    )) 
  {
    stop("return argument must be either NULL, 'aic', or 'bic'")
  }
  
  
  if (
    !(
      isTRUE(is.numeric(n)) |
      is.null(n))) 
  {
    stop("n arguement must either be NULL or an integer")
  }
  
  
  
  
  #once you add more information criteria, it would be nice if this process were 
  #executed via a function applied to a list however i dont really want to set that
  #up right now, would ideally set up some sort of helper function that is not
  #exported to the user
  
  outputTables <- vector(mode = "list", length = 2)
  names(outputTables) <- c("aic", "bic")
  
  
  #get top aic models
  aicTopInd <- order(object$aic)
  #create table of top models
  outputTables[[1]] <- data.frame(index = aicTopInd,
                                  model = object$models[aicTopInd] |>
                                    as.character(),
                                  aic = object$aic[aicTopInd])
  
  #get top aic models
  bicTopInd <- order(object$bic)
  #create table of top models
  outputTables[[2]] <- data.frame(index = bicTopInd,
                                  model = object$models[bicTopInd] |>
                                    as.character(),
                                  bic = object$bic[bicTopInd])
  
  
  
  
  
  
  #if there is a n arguement, subset the tables to only include the top n models
  if (!is.null(n)) {
    outputTables <- lapply(outputTables, head, n=n)
  }
  
  
  #if there is a toRetrun argeument, only return the tables for the IC asked for
  if (!is.null(toReturn)) {
    outputTables <- outputTables[toReturn]
  }
  
  
  
  
  return(outputTables)
}