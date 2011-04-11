#############################################################################
### DEVIANCE - ETA - PLOT
#############################################################################
#dep <- function(mymodel, id=c("all","none"), ...){
#standardGeneric("dep")
#}
#setGeneric("dep", def=dep)
#############################################################################
###                                                                   synonym
#############################################################################
#devianceResidual.linearPredictor <- dep
#############################################################################
###                                                                       GLM
#############################################################################
dep <- function(mymodel, id=c("all", "none"), ...){
### ================================= checking and argument matching
  id <- match.arg(id)
  f <- parseFormula(mymodel)
### ================================= calculation
  eta <- predict(mymodel, type="link")
  deviance.residuals <- residuals(mymodel)
### ================================= plot
  plot1 <- xyplot(deviance.residuals ~ eta,
                  main="dep.glm() \n deviance residuals vs. linear predictor",
                  xlab=expression(paste("linear predictor, ", hat(eta))),
                  ylab=expression(paste("deviance residuals, ", r[D]))
                  )
  print(plot1)
### ================================= IDENTIFICATION
  identifyControl(panel.matrix=trellis.currentLayout(),
                  original.row.names=row.names(f@data),
                  id=id)
}
### ================================= method
#setMethod("dep", "glm", dep.glm)
#############################################################################

