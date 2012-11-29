\name{ortho}
\alias{ortho}
\title{
Test a matrix of contrast coefficients for pairwise orthogonality
}
\description{
This function tests all pairwise contrast coefficients for
orthogonality.
The function tells the user which pair of contrasts are not orthogonal.
}
\usage{
ortho(contr, round = 10)
}
\arguments{
  \item{contr}{
    A matirx, wiht k-1 rows (one for each contrast) and k columns one
    for each level of a factor.
}
  \item{round}{
    To test if the sum of the products of the coefficients is zero the
    sum is rounded using this value as digits argument for round.
}
}
\details{
For all pairs of contrasts coefficients the sum of the products of the
coefficients is calculated and compared if it is zero or not.
}
\value{
 a logical
}
\references{
none
}
\author{
\email{thomas.fabbro@unibas.ch}
}
\note{
There are more elegant solutions to this problem but this one tells the
user which pairs of contrasts are not orthogonal.
}

\seealso{
  \code{\link{mancontr}}
}
\examples{
trt.contr <- rbind("control-sugar"=c(1, -1/4, -1/4, -1/4, -1/4),
                   "pure-mixed"=c(0, 1/3, 1/3, -1, 1/3),
                   "monosaccharides-disaccharides"=c(0,1/2,1/2,0,-1),
                   "gluc-fruc"=c(0,1,-1,0,0))
ortho(trt.contr)
}
\keyword{file}

