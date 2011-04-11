#############################################################################
### PARSE FORMULA
#############################################################################
## This generic function should extract a number of elements from a
## fitted-model-object. For a given class it should always provide the same
## elements (they are listed at the beginning of the corresponding definition)
##
## some definitions:
##
## 'terms' are the names of variables (response or predictos) WITH the transformation
## 'vars' are the names of the variables (response or predicotrs)
##        this distintion is only important for numeric variables,
##        therefore there should always be:
##        - terms.factor
##        - terms.numeric
##        - vars.numeric
##          (vars.factor would be the same because it is not possible to transform a factor)
##         
## ------------------------------------------------------------------
setClass(Class = "Formula",
         representation = representation(
           response.term = "character",
           response.var = "character",
           response.values = "numeric",
           predict.vars.numeric = "character",
           data = "data.frame"
           )
         )
## ------------------------------------------------------------------ CONSTRUCTOR (not to be called manually)
parseFormula <- function(mymodel, ...){
standardGeneric("parseFormula")
}
setGeneric("parseFormula", def=parseFormula)
#############################################################################
###                                                                        LM
#############################################################################
## terms.numeric
## response.var
## predictor.vars
parseFormula.lm <- function(mymodel, ...){
  ##  general comments on model objects
  #################################
  ## 1) data is in the 

  ## assumptions on the formula:
  ## 1) there is only one response term
  #################################
  ## 'vars' are the names of the variables (response & predicotrs)
  my.terms <- names(attr(mymodel$terms, "dataClasses"))
  my.vars <- all.vars(formula(mymodel)) # without transformation
  my.vars.class <- attr(mymodel$terms, "dataClasses")
    ## 'terms' are the names of all variables (response & predicots) WITH the transformation
  ##         e.g. log(Area)
  ## ----------- response
  response.var <- my.vars[1]
  response.term <- my.terms[1]
  ## ----------- predictors
  predict.terms <- my.terms[-1]
  predict.vars <- my.vars[-1]
  predict.vars.class <-   my.vars.class[-1]
  ##
  predict.vars.numeric <- predict.vars[predict.vars.class=="numeric"]# if none exists, e.g. in an ANOVA, it has lenght 0
  ## ----------- data
  data <- mymodel$model
  eval(parse(text=paste("response.values <- mymodel$model$", response.term, sep="")))
  ##
 # intercept.logical <<- as.logical(attr(mymodel$terms, "intercept"))
  ##
  f.lm <- new("Formula",
              response.term = response.term,
              response.var = response.var,
              response.values = response.values,
              predict.vars.numeric = predict.vars.numeric,
              data = data
      )
  return(f.lm)
}
setMethod("parseFormula", "lm", parseFormula.lm)

#############################################################################
###                                                                       GLM
#############################################################################
parseFormula.glm <- function(mymodel, ...){
  ## #################################
  ## general comments
  ## #################################
  ## returns 

  ## assumptions on the formula:
  ## 1) there is exactly one predictor
  #################################
  ## 'vars' are the names of the variables (response & predicotrs)
  my.vars <- all.vars(formula(mymodel)) # without transformation
  my.vars.class <- attr(mymodel$terms, "dataClasses")
  vars.numeric <- my.vars[my.vars.class=="numeric"]
  ## 'terms' are the names of all variables (response & predicots) WITH the transformation
  ##         e.g. log(Area)
  my.terms <- names(attr(mymodel$terms, "dataClasses"))
  terms.numeric <-  my.terms[my.vars.class=="numeric"]  # the numeric predictors
### response.var
  response.var <- my.vars[1]
### response.term
  response.term <- my.terms[1]
###
  predict.vars <- my.vars[-1]
  predict.vars.class <- my.vars.class[-1]
  predict.terms <- my.terms[-1]
  predict.terms.numeric <- predict.terms[predict.vars.class=="numeric"]  # the numeric predictors
### predict.vars.numeric
  predict.vars.numeric <- predict.vars[predict.vars.class=="numeric"] # if none exists, e.g. in an ANOVA, it has lenght 0
## ### predict.terms.numeric
##   predict.terms.numeric <<- predict.terms[predict.vars.class=="numeric"] 
## ### index_coef.terms.numeric
##   index_coef.terms.numeric <<- match(predict.terms.numeric, names(coef(mymodel)))
## ### mydata
 data <- mymodel$data
## ### response.values
 eval(parse(text=paste("response.values <- data$",response.var,sep="")))
## ### intercept
##   intercept.logical <<- as.logical(attr(mymodel$terms, "intercept"))
####
  f.glm <- new("Formula",
              response.term = response.term,
              response.var = response.var,
               response.values = response.values,
              predict.vars.numeric = predict.vars.numeric,
              data = data
              )
  return(f.glm)
}
setMethod("parseFormula", "glm", parseFormula.glm)


#############################################################################
###                                                                      LMER
#############################################################################
## parseFormula.lmer <- function(mymodel, ...){
## ### ===========notes:
##   ## all.vars(formula(mymodel)) : gives random and fixed factor
## ### mydata
##   my.data <- mymodel@frame # it is exported at the very end because there are columns that are added
##   ## response.values
##                                         #  eval(parse(text=paste("response.values <<- my.data$",response.var,sep="")))
##   ## intercept
##   intercept.logical <<- as.logical(attr(terms(mymodel), "intercept"))
##   ## all vars (fixed&random) without the intercept if present
##   vars <- all.vars(formula(mymodel))[-attr(attr(mymodel@frame, "terms"), "intercept")]
##   ##

## ### group
##   group.vars <<- unlist(lapply(mymodel@ST, dimnames))
##   effect.vars <<- unlist(mymodel@ST)  
## ### random
##   rand.terms <- names(mymodel@flist)
##   im <- match(vars, rand.terms)
##   rand.vars <- vars[im]
##   rand.vars <- rand.vars[!is.na(rand.vars)]
##   rand.vars <<- names(mymodel@flist)
##   ## nested
##   nested.index <- grep(":", rand.terms)
##   rand.vars.nested <- rand.terms[nested.index]
##   ## now we contrstuct these factors and add them to the data.frame
##   for.new <- strsplit(rand.vars.nested, ":")
##   for (j in seq(along=for.new)){ # elements in the list
##     fn <- sub("\\(|\\)", "", for.new[[j]])    
##     cc <- paste("my.data[,\"", fn, "\"]", sep="")
##     eval(parse(text=paste("my.data <- cbind(my.data, \"", rand.vars.nested[j],"\" = ",escapedDeparse2(cc),")", sep="")))
##   }
##   ## fixed terms without the intercept if present
##   fixed <- attr(terms(mymodel), "dataClasses")[-1]# to remove the response
##   fixed.terms.numeric <<- attr(fixed, "names")[fixed=="numeric"]
##   fixed.terms.factor <<- attr(fixed, "names")[fixed=="factor"]
##   ## ! a work around to get all fixed.vars.numeric
##   ## assuming that beside the intercept NO other numeric terms exist
##   terms.class <- attr(attr(mymodel@frame, "terms"), "dataClasses")
##   m <- match(vars, names(terms.class))
##   vars.class <- terms.class[m]
##   fixed.vars.numeric <- vars[vars.class=="numeric"]
##   fixed.vars.numeric <<- fixed.vars.numeric[!is.na(fixed.vars.numeric)]
##   ## export my.data
##   my.data<<-my.data
## }
## ### =========== method
## setMethod("parseFormula", "mer", parseFormula.lmer)
#############################################################################

## ------------------------------------------------------------------ METHODS

## a method that extracts from the class Formula
setMethod(
          f = "[",
          signature = c("Formula"),
          definition = function(x, i, j, drop){
            slot(x, i)
          }
          )
