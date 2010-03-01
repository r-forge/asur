\name{mancontr}
\alias{mancontr}
\alias{mycontr}
\title{Contrast Matrix Construction}
\description{
Helps you to construct a contrast matrix and naming the contrasts
}
\usage{
mancontr(contr = NULL, contr.names = NULL)
}
\arguments{
  \item{contr}{a list of contrasts (given as vectors) or a matrix with
    contrasts given in (k-1) rows or (k-1) columns (k is the number of
    levels of a factor)}
  \item{contr.names}{a list or vector of contrast names}
}
\details{
  For factors with more than two levels, comparisons among different levels are usually of 
  interest (also comparisons among means of different levels). Therefore
  you need to be able to set your contrasts manually. Because it is also
  important to give your contrasts meaningful names. This is especially
  easy with this function. You can either provide them as row.names
  directly in the contrast matrix or in separate vector or list.
}
\value{
A (k times k-1) matrix
}
\author{\email{thomas.fabbro@unibas.ch}}
\seealso{\code{\link{contrasts}}, \code{\link{C}}}
\examples{
data(pea)
trt.contr <- rbind("control-sugar"=c(1, -1/4, -1/4, -1/4, -1/4),
                   "pure-mixed"=c(0, 1/3, 1/3, -1, 1/3),
                   "monosaccharides-disaccharides"=c(0,1/2,1/2,0,-1),
                   "gluc-fruc"=c(0,1,-1,0,0))
model <- aov(length ~ trt,
             contrasts=list(trt=mancontr(contr=trt.contr)), data=pea)

### ALTERNATIVE formulations:
## contrasts in a list:
#   trt.contr <- list(c( 1, -1/4, -1/4, -1/4, -1/4),
#                     c(  0,  1/3,  1/3,   -1,  1/3),
#                     c(  0,  1/2,  1/2,   0,   -1),
#                     c(  0,    1,   -1,   0,    0))
## contrasts in a matrix 
#   trt.contr <- rbind(c( 1, -1/4, -1/4,-1/4, -1/4),
#                      c(  0,  1/3,  1/3,  -1,  1/3),
#                      c(  0,  1/2,  1/2,   0,   -1),
#                      c(  0,    1,   -1,   0,    0))
## names of contrasts in a list:
#  trt.contr.names=list("control-sugar", "pure-mixed",
#    "monosaccharides-disaccharides", "gluc-fruc")
## names of contrasts in a vector:
#   trt.contr.names=c("control-sugar", "pure-mixed",
# "monosaccharides-disaccharides", "gluc-fruc")
#model <- aov(length ~ trt,
#         contrasts=list(trt=mancontr(contr=trt.contr, contr.names=trt.contr.names)),
#         data=pea)
}
\keyword{file}
