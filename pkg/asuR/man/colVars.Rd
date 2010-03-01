\name{colVars}
\alias{colVars}
\title{Column variances}
\description{
Form column variances for numeric arrays
}
\usage{
colVars(x, na.rm = FALSE, dims = 1, unbiased = TRUE, SumSquares = FALSE, twopass = FALSE)
}
\arguments{
  \item{x}{A numeric array (or a dataframe to convert to a matrix)}
  \item{na.rm}{Logical: Remove NA's (not available values)}
  \item{dims}{Number of dimensions to sum over [colSums] or leave alone 
              [rowSums]. Only useful when x is a multidimensional array}
  \item{unbiased}{Logical: Use (N-1) in the denominator when calculating 
                  variance}
  \item{SumSquares}{Logical: If TRUE, colVars just returns sums of squares.}
  \item{twopass}{Logical: If TRUE, colVars uses the corrected two-pass 
     algorithm of Chan Golub & LeVeque, which is slower but less subject 
     to roundoff error.}
}
\value{
A vector with the variance for every column. 
}
\author{colVars: Originally by Douglas Bates <bates@stat.wisc.edu> as package 
       "MatUtils". Modified, expanded, and renamed by David Brahm 
       <brahm@alum.mit.edu>, with help of course from the R-help gurus.
}
\seealso{\code{\link{colMeans}}}
\examples{
mat <- cbind(rnorm(100, sd=sqrt(1)),
             rnorm(100, sd=sqrt(2)),
             rnorm(100, sd=sqrt(3)),
             rnorm(100, sd=sqrt(4)),
             rnorm(100, sd=sqrt(5)))
colVars(mat) 
}
\keyword{file}
\keyword{file}
