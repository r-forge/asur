#############################################################################
### LINEARIZEDRESPONSE - ETA - PLOT
#############################################################################
lep <- function(mymodel, id=c("all","none"), ...){
standardGeneric("lep")
}
setGeneric("lep", def=lep)
#############################################################################
###                                                                   synonym
#############################################################################
linearizedResponse.linearPredictor <- lep
#############################################################################
###                                                                       GLM
#############################################################################
### name:
### y (linearized response) against e (eta, the linear predictor)
###
lep.glm <- function(mymodel, id=c("all", "none"), ...){
### ================================= checking
  if( class(mymodel)[1]!="glm" ){stop("the model has to be of class glm")}
    id <- match.arg(id)
### ================================= reading
  parseFormula(mymodel)
##   predicted <- predict(mymodel)
##   response.var <- all.vars(formula(mymodel))[1]
##   mydata <- mymodel$data
##   ## the response values
##   to.eval <- paste("response.values <- mydata","$",response.var,sep="")
##   eval(parse(text=to.eval))
### ================================= calculation
  ## x values
  eta <- predict(mymodel, type="link")
  ## y values
  mu <- predict(mymodel, type="response")
  predicted <- predict(mymodel, type="link")
  linearizedResponse <- predicted + (response.values-mu)/mu
### ================================= plot
  for.ploting <- xyplot(linearizedResponse ~ eta,
                        main="lep.glm \n linearized response vs. linear predictor plot",
                        xlab=expression(paste("linear predictor ", hat(eta))),
                        ylab=paste("linearized response")
                        )
  print(for.ploting)
### ================================= identification
  identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=row.names(my.data), id=id)
##   if(id=="all"){
##     selection.message <- "By clicking on the (first) mouse button selected points are identified\n"
##     selection.message.continue <- "(click a mouse other than the first (or ESC) to continue)\n"
##     cat(selection.message)
##     cat(selection.message.continue)
##     identified <- NULL
##     trellis.focus("panel", 1, 1)
##     ## print(dat$name)
##     identified <- panel.identify(labels=rownames(dat))
##     return(identified)
##   }
}
### ================================= method
setMethod("lep", "glm", lep.glm)
#############################################################################
