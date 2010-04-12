## ------------------------------------------------------------------ exported
inspect <- function(mymodel, which = c("select", "sequence", "all"), id=c("all", "none"), ...){
standardGeneric("inspect")
}
setGeneric("inspect", def=inspect)
## ------------------------------------------------------------------ not exported
setGeneric(
           name = "long",
           def = function(object, ...){standardGeneric("long")}
           )

setGeneric(
           name = "short",
           def = function(object, ...){standardGeneric("short")}
           )

setGeneric(
           name = "f",
           def = function(object, ...){standardGeneric("f")}
           )

setGeneric(
           name = "id",
           def = function(object, ...){standardGeneric("id")}
           )
setGeneric(
           name = "id<-",
           def = function(x, value){standardGeneric("id<-")}
           )

setGeneric(
           name = "idList",
           def = function(object, ...){standardGeneric("idList")}
           )
