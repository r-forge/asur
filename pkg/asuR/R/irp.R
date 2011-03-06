irp <- function(mymodel, id=c("all", "none"), ...){
  ### ================================= checking and argument matching
  if(class(mymodel)[1]!="lm"){stop("ERROR: the model you want to inspect should be fitted with lm")}
  id <- match.arg(id)
  ### ================================= calculations
  ri <- rstudent(mymodel)
  index <- 1:length(ri)
  ### ================================= plot
  plot1 <- xyplot(ri~index,
                  main="irp \n index plot of stud. residuals",
                  xlab="INDEX",
                  ylab="studentized Residuals",
                  panel=myPanel <- function(x,y,...){
                    panel.xyplot(x,y,...)
                    panel.abline(a=0)
                    panel.loess(x,y,...)
                  }
                  )
  print(plot1)
  ### ================================= IDENTIFICATION
   identifyControl(panel.matrix=trellis.currentLayout(),
                  original.row.names=dimnames(model.matrix(mymodel))[[1]],
                  id=id
                  )
  ## identified <- NULL
  ## trellis.focus("panel", 1, 1)
  ## identified <- c(identified, panel.identify(labels=dimnames(model.matrix(mymodel))[[1]]))
  ## return(identified)
}
