#' bestSubsetSel - best subset selection
#' @details
#' this function fits a model for all possible variable combinations and returns the AIC and BIC. In the future, it would be nice to set this up for more types of models beyond lm and glm like mixed effects models as well as adding in different ICs.
#' @param dat a data frame containing the covariates under consideration as well as the response variable. Do not include superfluous variables.
#' @param respVar the name of the response variable in dat (just the var name, no need to use a string)
#' @param method the type of model to fit, either "lm" or "glm"
#' @param modelString an additional string to include in all of the model formula. This is useful for forcing certain variables into the model. Must be set up with a leading plus sign like "+varName". If theres nothing you want to force in, this should be NULL. Defaults to NULL. This does end up having the same term in the model multiple times in some of the fromula but this is not an issue, the term is still only estimated once. You can check with summary(lm(Sepal.Length ~ Sepal.Width+Sepal.Width, data = iris))
#' @param family distribution family for glm model. If not using method glm, then this should be NULL. Defaults to NULL
#' @param saveOutput a file path to a directory in which to save the outputs. Especially helpful if this is going to take a long time and you want to be able to pull the results in without having to rerun this function. It will save it in the specified directory named with the date and time. If you do not want to save the results, then this should be NULL. Defaults to NULL.
#' @return a list of the formula for all of the models fit, their associated IC, as well as some of the input arguements. Returns an object of class "bss" so this can be used with my summary.bss() s3.
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
#' @import rlang
#' @import stringr
#' @export

bestSubsetSel <- function(dat, respVar, method,
                          modelString = NULL, family = NULL,
                          saveOutput = NULL) {
  
  #checking function assumptions
  if (!(method %in% c("lm", "glm"))) {
    stop("method must be lm or glm")
  }
  if (method == "glm" & is.null(family)) {
    stop("must specify distribution family to use glm")
  }
  
  
  #
  respVar <- enquo(respVar)
  
  #get colnames of all but response variable
  allColNames <- colnames(dat)
  predNames <- allColNames[allColNames != as_name(respVar)]
  
  #create vectors of all possible predictor combinations
  predCombos <- unlist(lapply(1:length(predNames),
                              function(k) { 
                                #function that creates all combinations of 
                                #size k of the supplied column names
                                combn(predNames, k, simplify = FALSE)
                              }),
                       recursive = FALSE)
  #add in a null model
  predCombos <- c(predCombos, "1")
  #make these vectors into formula
  allFormula <- lapply(predCombos, 
                       function(predsi) {
                         paste(as_name(respVar), " ~ ",
                               paste(predsi, collapse = " + "),
                               modelString) |>
                           as.formula()
                       }
  )
  
  #fit each of the models but do not save the fit as this is too memory intensive
  #just save the information criteria along with the formulas
  
  aicHolder <- rep(NA, length = length(allFormula))
  bicHolder <- rep(NA, length = length(allFormula))
  
  pb <- txtProgressBar(min = 1, max = length(aicHolder), style = 3)
  
  if (method == "lm") {
    for (i in 1:length(aicHolder)) {
      modeli <- lm(allFormula[[i]], data = dat)
      aicHolder[i] <- modeli |> AIC()
      bicHolder[i] <- modeli |> BIC()
      setTxtProgressBar(pb, i)
    }
  } else if (method == "glm") {
    for (i in 1:length(aicHolder)) {
      modeli <- glm(allFormula[[i]], data = dat, family = family)
      aicHolder[i] <- modeli |> AIC()
      bicHolder[i] <- modeli |> BIC()
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  
  
  if (!is.null(saveOutput)) {
    styledTime <- Sys.time() |> 
      str_replace_all("[ :.-]", "_") |>
      str_remove("_[0-9]*$")
    aicFileName <- paste(styledTime, "finishedAic.RData", sep = "_")
    save(aicHolder, file = paste(saveOutput, aicFileName, sep = "/"))
    bicFileName <- paste(styledTime, "finishedBic.RData", sep = "_")
    save(bicHolder, file = paste(saveOutput, bicFileName, sep = "/"))
  }
  
  
  
  
  #setting up output for s3 summary function
  out <- list(models = allFormula,
              aic = aicHolder,
              bic = bicHolder, 
              method = method,
              family = family,
              modelString = modelString)
  class(out) <- "bss"
  return(out)
  
}




