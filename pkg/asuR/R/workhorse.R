#############################################################################
###                                                                 WORKHORSE
#############################################################################
workhorse <- function(mymodel,
                      which = c("select", "sequence", "all"),
                      id = c("all", "none"),
                      plotList = plotList){
  which <- match.arg(which)
  id <- match.arg(id)
  ##
#  print(plotList)
  switch(which,
         select = {                                                                       ## which = "select"
           repeat{
             ## asking the user
             user.selection <- menu(choices = long(plotList),
                                    title = "\n Select the number of the plot you want:"
                                    )
             ## the user selects now
             if(user.selection==0){break}
             thisPlot <- plotList[user.selection]
#             e.deside <- paste(short(plotList[user.selection]), ".id <- ", f(plotList[user.selection]),"(mymodel, id=\"",id,"\")",sep="")
             ##            eval(parse(text=e.deside))
             id(thisPlot) <- f(thisPlot)(mymodel, id = id, main = long(thisPlot))
             plotList[user.selection] <- thisPlot
           }
         },
                                                                                          ## which = "sequence"
         sequence = {#print(!is.numeric(id)); print(id=="none")
                     which.plots <- seq(along=row.names(plot.names))
                     for(i in seq(along=which.plots)){
                       e.ask <- paste("if(!is.numeric(id)){user.selection <- menu(choices=c(\"yes\",\"no\",\"stop\"), title=\"***\n",plot.names[i,1], "\n   (functions: ",plot.names[i,2]," or ",plot.names[i,3],")\")}else{user.selection <- 1}",sep="")
                       ##             e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{identified.list <- c(identified.list, list(",plot.names[i,2],".id = ", plot.names[i,2],"(mymodel, id=\"", id, "\")))}",sep="")
                       e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{",plot.names[i,2],".id <- ", plot.names[i,2],"(mymodel, id=\"",id,"\")}",sep="")
                       ##
                       eval(parse(text=e.ask))
                       eval(parse(text=e.deside))
                     }
                   },
                                                                                          ## which = "all"
         all = {#print(!is.numeric(id)); print(id=="none")
                if(which=="all"){which.plots <- seq(along=row.names(plot.names))}
                for(i in seq(along=which.plots)){
                  user.selection <- 1
                  e.deside <- paste("if(user.selection==0 | user.selection==3){break};if(user.selection==2){next}else{", plot.names[i,2],"(mymodel, id=\"none\")}",sep="")
                  ##
                  eval(parse(text=e.deside))
                }
              }
         )
  ##
  invisible(idList(plotList))
}

