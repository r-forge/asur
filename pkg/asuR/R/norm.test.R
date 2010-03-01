norm.test <- function(x){
  if(any(c("aov","lm")==class(x))){
    x <- rstudent(x)
  }
  if(!is.numeric(x)){stop("x has to be a model or a numeric vector")}
  ##
  dat <- cbind(rnorm(x),rnorm(x),rnorm(x),rnorm(x),rnorm(x),rnorm(x),rnorm(x),rnorm(x),x)
  ran <- sample(1:9)
  lis <- 1:9
  the.true.one <- lis[ran==9]
  dat <- dat[,ran]
  ##
  dat <- scale(dat)
  extrem <- max(abs(dat))
  ##
  old.par <- par()
  par(mfrow=c(3,3), mar=c(1,1,1,1), oma=c(4,4,0,0), col="black")
  for(i in 1:9){
    mat <- cbind(rep(1:3, each=3),rep(1:3, times=3))
    pushViewport(viewport(layout.pos.row=mat[i,1], layout.pos.col=mat[i,2]))
    ## 
    x.ax <- sort(dat[,i])
    y.ax <- qnorm((seq(along.with=x.ax)-0.5)/length(x.ax))
    ## all ploting
    plot(y.ax ~ x.ax, type="n", xlim=c(-extrem,extrem), ylim=c(-extrem,extrem), xlab="", ylab="", main="", axes=TRUE)
    lines(loess(y.ax ~ x.ax), col="red", lwd=2)
    abline(a=0,b=1)
    text(x=-extrem, y=extrem, label=paste("",as.character(i)), font=2, cex=2, adj=c(0,1))
    points(y.ax ~ x.ax,, col="blue")
  }
  mtext(text="sample quantiles", outer=TRUE,side=1, line=2, cex=1.5)
  mtext(text="theoretical quantiles", outer=TRUE, side=2, line=2, cex=1.5)
  ## restoring
    on.exit(par(old.par))
  ## User Interaction
  user.value <- readline(paste("Please enter the number of the panel containing your data: ", sep=""))
  if(is.na(as.numeric(user.value)) || !any(1:9==as.numeric(user.value))){stop("you have to enter a number between 1 and 9!")}
  user.value <- as.numeric(user.value)
  the.true.one.char <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")[the.true.one]
  if(user.value==the.true.one){
    cat(paste("Sorry, but you are right, keep on looking for a better transformation!\n(the ", the.true.one.char, " panel shows your data)\n", sep=""))
  }else{
    cat(paste("Congratulation, apparently you found a good transformation! \n(the ", the.true.one.char," panel shows your data)\n", sep=""))
  }
}
#############################################################################
## for backward compatibility only (there is an object norm also in package Matrix)
## probably not needed any more
## norm <- norm.test 
