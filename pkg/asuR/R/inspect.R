### ------------------------------------------------------------------ CLASSES
setClass(Class = "pDesc",
         representation = representation(
           longName = "character",
           shortName = "character",
           id = "integer", # identified data
           f = "function"
           )
         )

setClass(Class = "pDescList",
         representation = representation(
           list = "list"
           )
         )

### ------------------------------------------------------------------ METHODS
setMethod(
          f = "length",
          signature = c("pDescList"),
          definition = function(x){
            return(length(x@list))
          }
          )

setMethod(
          f = "long",
          signature = c("pDescList"),
          definition = function(object, ...){
            return(sapply(object@list, function(x)x@longName))
          }
          )
setMethod(
          f = "long",
          signature = c("pDesc"),
          definition = function(object, ...){
            return(object@longName)
          }
          )

setMethod(
          f = "short",
          signature = c("pDescList"),
          definition = function(object, ...){
            return(sapply(object@list, function(x)x@shortName))
          }
          )
setMethod(
          f = "short",
          signature = c("pDesc"),
          definition = function(object, ...){
            return(object@shortName)
          }
          )
setMethod(
          f = "f",
          signature = c("pDesc"),
          definition = function(object, ...){
            return(object@f)
          }
          )
setMethod(
          f = "[",
          signature = c("pDescList"),
          definition = function(x, i, j, drop){
            x@list[[i]]
          }
          )
setReplaceMethod(
          f = "[",
          signature = c("pDescList"),
          definition = function(x, i, j, value){
            x@list[[i]] <-  value
            return(x)
          })

setMethod(
          f = "id",
          signature = c("pDesc"),
          definition = function(object, ...){
            id <- object@id
            return(id)
          })
setReplaceMethod(
          f = "id",
          signature = c("pDesc"),
          definition = function(x, value){
            x@id <-  value
            return(x)
          })

setMethod(## creates a list of identified values, for every plot one slot
          ## and one slot with all values that were selected at least once
          f = "idList",
          signature = c("pDescList"),
          definition = function(object, ...){
            identifiedList <- lapply(object@list, function(x){x@id})
            names(identifiedList) <- short(object)
            identified.all <- sort(unique(unlist(identifiedList)))
            identifiedList <- c(identifiedList, list(all=identified.all))
            return(identifiedList)
          }
          )

#############################################################################
###                                                                        LM
#############################################################################
inspect.lm <- function(mymodel, which = c("select", "sequence", "all"), id=c("all", "none"), ...){
  if(any(is.nan(rstudent(mymodel)))){stop("Residuals could not be studentized!!")}

  ##
  ## here is the place to add additional fuctions that should become available for inspecting an lm object
  plotList <- new("pDescList",
                  list = list(
                    new("pDesc", longName = "Potential  vs. studentized residuals", shortName = "prp", f = prp, id = as.integer(NA)),
                    new("pDesc", longName = "Studentized residuals vs. predicted values", shortName = "ryp", f = ryp, id = as.integer(NA)),
                    new("pDesc", longName = "Index  vs. residual", shortName = "irp", f = irp, id = as.integer(NA)),
                    new("pDesc", longName = "Index  vs. Hadi's influence measure", shortName = "ihp", f = ihp, id = as.integer(NA)),
                    new("pDesc", longName = "Index  vs. leverage", shortName = "ilp", f = ilp, id = as.integer(NA))
                    )
                  )

  workhorse(mymodel = mymodel, which = which, id = id, plotList = plotList)
  
}
setMethod("inspect", "lm", inspect.lm)

#############################################################################
###                                                                       GLM
#############################################################################
inspect.glm <- function(mymodel, which = c("select", "sequence", "all"), id=c("all", "none"), ...){
  ## here is the place to add additional fuctions that should become available for inspect
  plotList <- new("pDescList",
                  list = list(
                    new("pDesc", longName = "deviance residuals vs. linear predictor", shortName = "dep", f = dep, id = as.integer(NA)),
##                     new("pDesc", longName = "partial residual vs. each predictor", shortName = "rpp", f = rpp, id = as.integer(NA)),
                    new("pDesc", longName = "linearized response vs. linear predictor", shortName = "lep", f = lep, id = as.integer(NA)),
                    new("pDesc", longName = "half-normal quantiles vs. absolute stud. residuals", shortName = "hnp", f = hnp, id = as.integer(NA))
                    )
                  )
 
  workhorse(mymodel = mymodel, which = which, id = id, plotList = plotList)

}

setMethod("inspect", "glm", inspect.glm)
## #############################################################################
## ###                                                                       GLM
## #############################################################################
## inspect.glm <- function(mymodel, which=c("select", "sequence", "all"), id=c("all", "none"), ...){
##  #################################
## ### here is the place to add additional fuctions that should become available for inspect
##   plot.names <- data.frame(rbind(
##                                  cbind(fullname="deviance residuals vs. linear predictor",         short.function="dep",         long.function="devianceResidual.linearPredictor"),
##                                  cbind(fullname="partial residual vs. each predictor",             short.function="rpp",          long.function="partialResidual.eachPredictor" ),
##                                  cbind(fullname="linearized response vs. linear predictor",         short.function="lep",         long.function="linearizedResponse.linearPredictor"),
##                                  cbind(fullname="half-normal quantiles vs. absolute stud. residuals", short.function="hnp",         long.function="halfNormalQuantiles.absoluteStudentizedResiduals")
##                                  ## cbind(fullname="",         short.function="",         long.function=""),
##                                  ))
## #################################
##   workhorse(mymodel=mymodel, which=which, id=id, plot.names=plot.names)
## }
## setMethod("inspect", "glm", inspect.glm)
## #############################################################################
## ###                                                                      LMER
## #############################################################################
## inspect.lmer <- function(mymodel, which=c("select", "sequence", "all"), id=c("all", "none"), ...){
## #################################
## ### here is the place to add additional fuctions that should become available for inspect
##   plot.names <- data.frame(rbind(
##                                  cbind(fullname="Normal quantile quantile plot of residuals by levles of categorical fixed effects",         short.function="nrp",         long.function="NormalQuantiles.Residuals.CategoricalFixedEffects"),
##                                  cbind(fullname="Box- or dotplot of residuals by levels, for each random factor",         short.function="rgp",         long.function="residuals.by.groups"),
##                                  cbind(fullname="Residuals vs. fitted values for all categorical fixed effects",         short.function="rfp",         long.function="Residuals.Fitted.CategoricalFixedEffects"),
##                                  cbind(fullname="Normal quantile quantile plot of random effects, for each factor",         short.function="nep",         long.function="NormalQuantiles.RandomEffectsQuantiles")
##                                  ## cbind(fullname="",         short.function="",         long.function=""),
##                                  ))
## #################################
##   workhorse(mymodel=mymodel, which=which, id=id, plot.names=plot.names)
## }
## setMethod("inspect", "mer", inspect.lmer)
#setMethod("inspect", "glmer", inspect.lmer)
#############################################################################
###                                                                       END
#############################################################################
