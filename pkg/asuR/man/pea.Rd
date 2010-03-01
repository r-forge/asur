\name{pea}
\alias{pea}
\docType{data}
\title{Pea data}
\description{
The effect of different sugars on length, in ocular units (x0.114=mm),
of pea sections grown in tissue culture with auxin present.
}
\usage{data(pea)}
\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{trt}}{a factor with levels \code{cont}: control,
      \code{fruc}: 2\% fuctose added, \code{gluc}: 2\% glucose added,
      \code{glucfruc}: 1\% glucose and 1\% fructose added,  \code{sucr}:
    2\% sucrose added}
    \item{\code{length}}{a numeric vector}
  }
}
\source{
Sokal, R. R. & Rohlf, J. F. (1995): \emph{Biometry: the principles and practice of 
statistics in biological research.} Freeman, New York (p 218).
}
\examples{
% ## created by 
% # cont <- c(75,67,70,75,65,71,67,67,76,68)
% # gluc <- c(57,58,60,59,62,60,60,57,59,61)
% # fruc <- c(58,61,56,58,57,56,61,60,57,58)
% # glucfruc <- c(58,59,58,61,57,56,58,57,57,59)
% # sucr <- c(62,66,65,63,64,62,65,65,62,67)
% # pea <- data.frame(
% #        trt=factor(rep(c("cont", "gluc", "fruc", "glucfruc", "sucr"), each=10),
% #                   levels=c("cont", "gluc", "fruc", "glucfruc", "sucr"),
% #                   ordered=FALSE), length=c(cont, gluc, fruc, glucfruc, sucr))
data(pea)
}
\keyword{datasets}
