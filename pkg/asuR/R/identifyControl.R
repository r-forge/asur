identifyControl <- function(panel.matrix, original.row.names, id, slot.names=NULL){
  verbose <- FALSE
  r.dim <- dim(panel.matrix)[1]; if(verbose){print(paste("VERBOSE:", r.dim))}
  c.dim <- dim(panel.matrix)[2]; if(verbose){print(paste("VERBOSE:", c.dim))}
  n.panels <- length(as.vector(panel.matrix)[as.vector(panel.matrix!=0)]); if(verbose){print(paste("VERBOSE n.panels", n.panels))}
  if(verbose){print(paste("VERBOSE panel.matrix:", panel.matrix))}
  ## checking the validity of the id argument given
  if(is.character(id)){
    if(length(id)!=1){stop("if 'id' is given as a character it should have lenght one")}
    if(!(id=="all"|id=="none")){
      stop("the argument 'id' can be given as a character vector but must be either \"all\", \"none\"")
    }
  }#   if(!(id=="all"|id=="none"|(all(is.numeric(id))&length(id)==2))){stop("id has to be either \"all\", \"none\", or a numberic vector 'c(row, number)'")}
  if(is.numeric(id)){
    if(length(id)!=2){stop("if the argument 'id' is given as a numeric vector, it should have length two, 'c(row, number)'")}
    if((panel.matrix==0)[rbind(id)]){
      warning(paste("the argument 'id' you gave me ask for identification in a panel that is empty, the argument \"all\" will be selected"))
      id <- "all"
    }
  }
  ##
  identified <- NULL
  selection.message <- "click the (first) mouse button to label points\n   (row names are shown in the plot)\n   (row numbers can be extracted by assigning this call to an object)\nclick a mouse button other than the first (or ESC) to continue\n"
  ##  selection.message <- "The row number of those points you select will be returned\n"
  if(is.numeric(id)){
    cat(selection.message)
    trellis.focus("panel", id[2], id[1])
    identified <- suppressWarnings(panel.identify(labels=original.row.names))
#    return(as.numeric(levels(as.factor(identified)))) #controls that every "number" enters just once!
    invisible(identified)
  }else{
    if(id=="none"){}
    if(id=="all"){
      if(verbose){print(paste("VERBOSE id:", id))}
      for (j in 1:r.dim){ #moves across panel cols
        for (i in 1:c.dim){ #moves across panel rows
          if(verbose){print(paste("VERBOSE: j,i", c(j,i)))}
          current.panel <- (j-1)*c.dim + i
          if(current.panel <= n.panels){
            cat(selection.message)
            trellis.focus("panel", i, j)
            if(n.panels==1){## only if there is more than one panel identified the results should be in a list, otherwise in a vector
              current.identified <- suppressWarnings(panel.identify(labels=original.row.names))
            }else{
              current.identified <- list(suppressWarnings(panel.identify(labels=original.row.names)))
            }
            identified <- c(identified, current.identified)
          }
        }
      }
    }
  }

   invisible(identified)
}
