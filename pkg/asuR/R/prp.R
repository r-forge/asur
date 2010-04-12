prp <- function(mymodel, id=c("none", "all"), ...){
  ## ================================= checking and argument matching
  if(class(mymodel)[1]!="lm"){stop("ERROR: the model you want to inspect should be fitted with lm")}
  f <- parseFormula(mymodel)
  if( length(f@predict.vars.numeric) == 0){stop("ERROR: the model you want to inspect should have at least one continuous predictor")}
  id <- match.arg(id)
  ## ================================= calculations
  ei <- rstudent(mymodel)
  k <- dim(model.matrix(mymodel))[2]-1 #minus 1 for the intercept
  pii <- hat(model.matrix(mymodel))
  di <- ei/(t(ei)%*%ei)
  ## calculations acording to Hadi-Script 5.68
  R <- k/(1-pii) * di^2/(1-di^2)
  P <- pii/(1-pii)
  ### ================================= plot
  plot.PR <- xyplot(R ~ P,
                    main="prp \n potential-residual plot",
                    xlab="potential",
                    ylab="residual",
                    )
  ## giving out the object
  print(plot.PR)
  identified <- NULL
  trellis.focus("panel", 1, 1)
  grid.lines(x=unit(c(0.5,0.5), "npc"), y=unit(c(0,1), "npc"))
  grid.lines(x=unit(c(0,1), "npc"), y=unit(c(0.5,0.5), "npc"))
  identified <- panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
  upViewport()
  grid.text("labels within the plotting region refer to the row names in the data.frame", x=unit(0.5, "npc"), y=unit(0.01, "npc"))
### ================================= IDENTIFICATION
##   identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=row.names(my.data), id=id)
## }
  switch(id,
         "all" = {

  
           return(identified)
         },
         "none" = {
           return(as.integer(NA))
         }
         )
}
