#############################################################################
### HALF - NORMAL - PLOT
#############################################################################
hnp <- function(mymodel, id=c("all","none"), ...){
standardGeneric("hnp")
}
setGeneric("hnp", def=hnp)
#############################################################################
###                                                                   synonym
#############################################################################
halfNormalQuantiles.absoluteStudentizedResiduals <- hnp
#############################################################################
###                                                                       GLM
#############################################################################
hnp.glm <- function(mymodel, id=c("all", "none"), ...){
### ================================= checking
  id <- match.arg(id)
### ================================= reading
  parseFormula(mymodel)
  ##
  ri <- rstudent(mymodel)
### ================================= calculatiion
  absStudentResiduals <- sort(abs(ri))
  halfNormalQuantiles <- qnorm(0.5*(1+((seq(along.with=absStudentResiduals)-(3/8))/(length(absStudentResiduals)+0.25)))) # p.24 stat. experimental design script
  ## data
  dat <- data.frame(absStudentResiduals=absStudentResiduals, halfNormalQuantiles=halfNormalQuantiles) # check the names they are important for identification
### ================================= plot
  hnp.plot <- xyplot(halfNormalQuantiles ~ absStudentResiduals, data=dat,
                     main="nrp.glm \n half-normal plot of absolute stud. residuals",
                     xlab="absolute value of studentized residuals",
                     ylab="half-normal quantiles",
                     panel=myPanel <- function(x,y,...){
                       panel.xyplot(x,y,...)
                       panel.loess(x,y,...)
                     }
                     )
  print(hnp.plot)
### ================================= identification
    identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=row.names(dat), id=id)
}
### ================================= method
setMethod("hnp", "glm", hnp.glm)
#############################################################################
