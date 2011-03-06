ilp <- function(mymodel, id=c("all", "none"), ...){
  ### ================================= checking and argument matching
  if(class(mymodel)[1]!="lm"){stop("ERROR: the model you want to inspect should be fitted with lm")}
  id <- match.arg(id)
  ### ================================= calculations
  pii <- hat(model.matrix(mymodel))
  index <- 1:length(pii)
  k <- dim(model.matrix(mymodel))[2]
  n <- dim(model.matrix(mymodel))[1]
  th <- 2*k/n
  ### ================================= plot
  plot1 <- xyplot(pii~index, ...,
                  xlab="INDEX",
                  ylab="Leverage",
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
  ##           return(as.integer(NA))
  ##        }
  ##        )
}
