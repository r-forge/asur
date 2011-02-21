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
             if(user.selection == 0){break}
             thisPlot <- plotList[user.selection] # thisPlot is an object of class pDesc
             id(thisPlot) <- f(thisPlot)(mymodel, id = id, main = long(thisPlot)) # changing the id slot of the pDesc object
             plotList[user.selection] <- thisPlot # replacing the original pDesc in the pDescList with the new onw this way the id slot is changed in the pDescList as well
           }
           
         },
                                                                                          ## which = "sequence"
         sequence = {

           ## looping through all possible plots after asking if the plot is wanted
           for(i in seq(along = long(plotList))){
             if(!is.numeric(id)){
               user.selection <- menu(choices=c("yes", "no", "stop"), title=paste("***\n", long(plotList)[i]))
             }else{
               user.selection <- 1
             }
             if(user.selection==0 | user.selection==3){break}
             if(user.selection==2){next}else{
               thisPlot <- plotList[user.selection] # thisPlot is an object of class pDesc
               id(thisPlot) <- f(thisPlot)(mymodel, id = id, main = long(thisPlot))
               plotList[user.selection] <- thisPlot
             }
           }
           
         },
                                                                                          ## which = "all"
         all = {

           ## looping through all possible plots
           for(i in seq(along = long(plotList))){
               thisPlot <- plotList[i] # thisPlot is an object of class pDesc
               id(thisPlot) <- f(thisPlot)(mymodel, id = id, main = long(thisPlot))
               plotList[user.selection] <- thisPlot
             }
           
              }
         )
  ##
  invisible(idList(plotList))
}

