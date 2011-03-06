ihp <- function(mymodel, id = c("all", "none"), ...){
  ### ================================= checking and argument matching
  if(class(mymodel)[1] != "lm"){stop("ERROR: the model you want to inspect should be fitted with lm")}
  id <- match.arg(id)
  ### ================================= calculations
  ei <- rstudent(mymodel)
  k <- dim(model.matrix(mymodel))[2]-1 #minus 1 for the intercept
  pii <- hat(model.matrix(mymodel))
  di <- ei/(t(ei)%*%ei)
  ## calculations acording to Hadi-Script 5.68
  R <- k/(1-pii) * di^2/(1-di^2)
  P <- pii/(1-pii)
  Hadi <- R+P
  index <- 1:length(pii)
  ## ================================= plot
  plot1 <- xyplot(Hadi~index, ...,
                  xlab="INDEX",
                  ylab="Hadi's influence",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
### ================================= IDENTIFICATION
   identifyControl(panel.matrix=trellis.currentLayout(),
                  original.row.names=dimnames(model.matrix(mymodel))[[1]],
                  id=id
                  )
  ## switch(id,
  ##        "all" = {
  ##          identified <- NULL
  ##          trellis.focus("panel", 1, 1)
  ##          identified <- panel.identify(labels=dimnames(model.matrix(mymodel))[[1]])
  ##          return(identified)
  ##        },
  ##        "none" = {
  ##          return(as.integer(NA))
  ##        }
  ##        )
}
