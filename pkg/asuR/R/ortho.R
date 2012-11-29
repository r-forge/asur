ortho <- function(contr, round = 10){
  n <- nrow(contr)
  mcol <- matrix(1:n, nrow = n, ncol = n)
  mrow <- matrix(1:n, nrow = n, ncol = n, byrow = TRUE)
  row1 <- mcol[upper.tri(mcol)]
  row2 <- mrow[upper.tri(mrow)]
  which <- vector(mode = "logical", length = n*(n-1)/2)
  for (i in seq(along = which)) {
    if( round(sum(contr[row1[1],] * contr[row2[1],]), round) ){
      which[i] <- TRUE
    }
  }
  if (all(!which)) {
    cat("All pairs of contrast coefficients are orthogonal.\n")
  }else{
    cat(paste("The coefficients of the following contrasts are not orthogonal: \n"))
    cat(paste(row.names(contr)[row1[which]], "   vs.   ", row.names(contr)[row2[which]], collaps = "\n", sep = ""))
  }
  return(all(!which))
}
