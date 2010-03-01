mancontr <- function(contr=NULL, contr.names=NULL){
### contrasts given as a list
  if(is.list(contr)){
    Cp <- NULL
    for (i in 1:length(contr)){
      if(length(contr[[i]])!=length(contr[[1]])){stop("The ", i,"th contrasts does not have the same number of elements as the contrast(s) before!")}
      Cp <- rbind(Cp, contr[[i]])
    }
  }
### a single contrast given as a vector
  if(!is.list(contr) && is.vector(contr)){
    contr <- matrix(contr, nrow=1)
  }
### contrasts given as a matrix
  if(is.matrix(contr)){
    if(dim(contr)[2]-dim(contr)[1] != -1 && dim(contr)[1]>=dim(contr)[2]){
      stop("The contrast matrix you provided does not have the right dimensions")
    }
    ##  (k-1 times k)
    if(dim(contr)[1]<dim(contr)[2]){
      Cp <- contr
    }
    ##
    ##  (k times k-1)
    if(dim(contr)[2]-dim(contr)[1]==-1){
      Cp <- t(contr)
      print(paste("The contrast matrix you provided had the dimensions", dim(contr)[1]," x ", dim(contr)[2],"; it was transposed"))
    }
  }
### contrasts CONSTRUCTION
  Ca <- ginv(Cp) ## the function ginv is from package MASS
### Contrast names
  if(is.null(contr.names)){
    Cnames <- rownames(contr)
  }
  if(is.null(contr.names)&&is.null(Cnames)){
    print("It is a good practice to give your contrasts a name!")
  }else{
    ## contr.names provided
    if(!is.null(contr.names)){
      if(!is.list(contr.names) && !is.vector(contr.names)){
        stop("The names for contrasts should be provided as a vector or a list")
      }else{
        contr.names <- paste("",contr.names, sep="")# only for readability
        if(length(contr.names)!=dim(Cp)[1]){
          stop("The number of contrasts should be the same as the number of contrast-names!")
        }else{
          if(is.list(contr.names)){
            Cnames <- contr.names[[1]]
            for(i in 2:length(contr.names)){
              Cnames <- c(Cnames,contr.names[[i]])
            }
          }else{
            Cnames <- contr.names
          }
        }
      }
    }
    ## attaching the names
    dimnames(Ca) <- list(paste( "V", 1:nrow(Ca), sep=""), paste("::",Cnames,sep=""))
  }
  Ca
}
#############################################################################
mycontr <- mancontr #back-compatibility only
