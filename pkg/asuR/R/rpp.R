#############################################################################
### RESPONSE - PREDICOTOR - PLOT
#############################################################################
rpp <- function(mymodel, id=c("all","none"), ...){
standardGeneric("rpp")
}
setGeneric("rpp", def=rpp)
#############################################################################
###                                                                   synonym
#############################################################################
partialResidual.eachPredictor <- rpp
#############################################################################
###                                                                        LM
#############################################################################
rpp.lm <- function(mymodel, id=c("all","none"), ...){
  id <- match.arg(id)
### ================================= reading
  parseFormula(mymodel)
### ================================= CALCULATIION
  dataX <- data.frame()
  if(length(predict.vars.numeric)==0){stop("your model does not contain a numeric predictor")}
  for(this.numeric.predictor in predict.vars.numeric){
    x.values <- response.values.lm
    y.values <- rstudent(mymodel)
    dataX <- rbind(dataX, data.frame(x.values=scale(as.numeric(x.values)),
                                     y.values=scale(as.numeric(y.values)),
                                     name=rep(this.numeric.predictor, times=length(x.values)),
                                     original.row.names=names(y.values)))
  }
### ================================= CORE
  ## panel arrangement
  n.panels <- length(predict.vars.numeric) #number of panesl
  r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
  c.dim <- ceiling(n.panels/r.dim) #number of panel cols
  ##
  respred <- xyplot(y.values~x.values|name, data=dataX,#layout=c(c.dim,r.dim),
                    main="rpp \n stud. residuals vs. each predictor",
                    pch=1, cex=1.5,
                    xlab="SCALED Predictors",
                    ylab="stud. residuals",
                    as.table=TRUE,
                    panel=myPanel <- function(x,y,...){
                      panel.xyplot(x,y,...)
                      panel.abline(a=0)
                      panel.loess(x,y,...)
                    }
                    )
  print(respred)
### ================================= IDENTIFICATION
  identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=dataX$original.row.names, id=id)
}
###
setMethod("rpp", "lm", rpp.lm)

#############################################################################
###                                                                       GLM
#############################################################################
rpp.glm <- function(mymodel, id=c("all","none"), ...){
  verbose <- FALSE
  ## to do:
  ## 1) ploting the fitted line with abline
  ### ================================= checking
  if( class(mymodel)[1]!="glm" ){stop("the model has to be of class glm")}
  id <- match.arg(id)
### ================================= PANEL ARANGEMENT
##   n.panels <- length(levels(dataX$name)) #number of panesl
##   r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
##   c.dim <- ceiling(n.panels/r.dim) #number of panel cols
### ================================= reading
  parseFormula(mymodel)
  mu <- predict(mymodel, type="response")
  attach(my.data)
  for(index.this.predict in seq(along=predict.terms.numeric)){
    this.predict <- predict.terms.numeric[index.this.predict]
    x <- match(names(coef(mymodel)), this.predict)
    this.predict.i <- c(1:length(x))[x==1&!is.na(x)]
    x.values <- eval(parse(text=this.predict))  # needs mydata to be attached!
    y.values <- (response.values-mu)/mu + coef(mymodel)[this.predict.i]*x.values
#    plot.eval <- paste("plot_",index.this.predict,"<-xyplot(y.values~x.values, data=my.data, main=\"rpp.glm() \n partial residuals vs. each predictor\", pch=1, cex=1.5, xlab=\"scaled predictor\", ylab=\"partial residual\", panel=myPanel <- function(x,y, ...){panel.xyplot(x,y,...); panel.abline(a=0,b=",coef(mymodel)[this.predict],", ...); panel.loess(x,y,...)} )", sep="")
        plot.eval <- paste("plot_",index.this.predict,"<-xyplot(y.values~x.values, data=my.data, main=\"\", pch=1, cex=1.5, xlab=\"\", ylab=\"\", panel=myPanel <- function(x,y, ...){panel.xyplot(x,y,...); panel.abline(a=0,b=",coef(mymodel)[this.predict],", ...); panel.loess(x,y,...)} )", sep="")
#    plot.name.eval <- paste("plot.name_",index.this.predict,"<-grid.text(\"", this.predict, "\", y=unit(0.95, \"npc\"))",sep="")
    eval(parse(text=paste(plot.eval)))
  }
  detach(my.data, pos=2)
    ### ================================= PANEL ARANGEMENT
  n.panels <- length(predict.terms.numeric) #number of panesl
  if(n.panels<1){stop("ERROR: there should be at least one numeric predictor")}
  r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
  c.dim <- ceiling(n.panels/r.dim) #number of panel cols
  panel.matrix <- matrix(c(1:n.panels, rep(0, r.dim*c.dim-n.panels)), nrow=r.dim, ncol=c.dim, byrow=TRUE)
  panel.matrix.logical <- panel.matrix>0
 # x11()
  grid.newpage()
  ##
  x.label <- "predictor"
  y.label <- "partial residual"
  ##
  pushViewport(viewport())
  grid.text("rpp.glm(), partial residuals vs. each predictor", y=unit(0.95, "npc"))
  grid.text(x.label, y=unit(0.05, "npc"))
  grid.text(y.label, x=unit(0.05, "npc"), rot=90)
  
  pushViewport(viewport(width=unit(0.9, "npc"), height=unit(0.9, "npc"), just=c("center", "center"), layout=grid.layout(r.dim,c.dim)))
#    pushViewport(viewport(x=unit(4, "lines"), y=unit(4, "lines"), just=c("left", "bottom"), layout=grid.layout(r.dim,c.dim)))
  for (m in 1:r.dim){
    for (n in 1:c.dim){
      if(panel.matrix.logical[m,n]){
        pushViewport(viewport(layout.pos.col=n, layout.pos.row=m))
        eval(parse(text=paste("grid.text(\"",predict.terms.numeric[panel.matrix[m,n]],"\",y=unit(0.9, \"npc\"))",sep="")))
        eval(parse(text=paste("print(plot_",panel.matrix[m,n],", newpage=FALSE)",sep="")))
        upViewport()
        }
      }
    }

##   predicted.response <- predict(mymodel, type="response")
##   ##
##   my.vars <- all.vars(formula(mymodel))
##   class.vars <- attr(mymodel$terms, "dataClasses")
##   terms <- names(attr(mymodel$terms, "dataClasses")) # they have the transformation ! e.g. log(Area)
##   ## variables without the transformation
##   response.var <- my.vars[1]
##   numeric.vars <- my.vars[class.vars=="numeric"]
##   ## terms with transformation
##   numeric.terms <-  terms[class.vars=="numeric"]  # the numeric predictors
##   predict.numeric.terms <- numeric.terms[-1]
##   ##
##   mydata <- mymodel$data
##   to.eval <- paste("response.values <- mydata","$",response.var,sep="")
##   eval(parse(text=to.eval))
##   ### ================================= CALCULATIION
##   mu <- predict(mymodel, type="response")
##   ##
##   attach(my.data)
##   ##
##   dataX <- data.frame()
##   coef <- vector()
##  for(this.predict in predict.terms.numeric){
   
##     ## print(this.predict)
##     ## finding the index of the current predictor
##     x <- match(names(coef(mymodel)), this.predict)
##     this.predict.i <- c(1:length(x))[x==1&!is.na(x)]
##     ##
##     x.values <- eval(parse(text=this.predict))  # needs mydata to be attached!
##     y.values <- (response.values-mu)/mu + coef(mymodel)[this.predict.i]*x.values
##     ##    x.values <- residuals(mymodel, type="partial")[,this.predict.i]
##     ## print(length(x.values))
##     ## print(length(y.values))
##     ## plot(x.values~y.values)
##     ## print(names(y.values))
    
##     dataX <- rbind(dataX, data.frame(x.values=scale(as.numeric(x.values)), y.values=scale(as.numeric(y.values)), name=rep(this.predict, times=length(x.values)),  original.row.names=names(y.values)))
##     ## x.values
##     ## y.values
##     ## name: for the strip name
##     ## original.row.names: the name of the rows in the original data frame , they are needed to be returned later
##     coef <- c(coef, coef(mymodel)[this.predict.i])
##   }
##   if(verbose){print(paste("VERBOSE names(dataX):", names(dataX)))}
##   detach(my.data, pos=2)
## ### ================================= PANEL ARANGEMENT
##     n.panels <- length(levels(dataX$name)) #number of panesl
##     r.dim <- ceiling(sqrt(n.panels)) #number of panel rows
##     c.dim <- ceiling(n.panels/r.dim) #number of panel cols
## #    panel.matrix <- matrix(names.y.values, nrow=r.dim, ncol=c.dim
##     ##   for.ploting <- xyplot(y.values ~ x.values|name,
##     ##                         xlab=paste("linear predictor"),
##     ##                         ylab=paste("linearized response"),
##     ##                         data=dataX             
##     ##                         )
##     ##   print(for.ploting)
##   ### ================================= PLOT
##   plot1 <- xyplot(y.values~x.values|name, data=dataX,layout=c(c.dim,r.dim),
##                   main="rpp.glm() \n partial residuals vs. each predictor",
##                   pch=1, cex=1.5,
##                   xlab="scaled predictor",
##                   ylab="partial residual",
##                   as.table=TRUE,
##                   panel=myPanel <- function(x,y, ...){
##                     panel.xyplot(x,y,...)
##                     panel.abline(0,coef(mymodel)[name])
##                     panel.loess(x,y,...)
##                   }
##                   )
##   print(plot1)
### ================================= IDENTIFICATION
##  identifyControl(panel.matrix=trellis.currentLayout(), original.row.names=dataX$original.row.names, id=id)
 ##  identified <- NULL
##   selection.message <- "The row names of the points you select will be returned\n"
##   if(is.numeric(id)){
##     if(length(id)==2 && id[2]<=r.dim && id[1]<=c.dim){
##       cat(selection.message)
##       trellis.focus("panel", id[1], id[2])
##       identified <- panel.identify(labels=dataX$names)
##       return(as.numeric(levels(as.factor(identified)))) #controls that every "number" enters just once!
##     }else{stop("wrong id specified!")}
##   }else{
##     if(id=="none"){}
##     if(id=="all"){
##       for (j in 1:r.dim){ #moves across panel cols
##         for (i in 1:c.dim){ #moves across panel rows
##           current.panel <- (j-1)*c.dim + i
##           if(current.panel <= n.panels){
##             cat(selection.message)
##             trellis.focus("panel", i, j)
##             current.identified <- list(panel.identify(labels=dataX$names))
##             identified <- c(identified, current.identified)
##           }
##         }
##       }
##     }
##   }
##   return(NULL) #identified
}
 
###
setMethod("rpp", "glm", rpp.glm)
