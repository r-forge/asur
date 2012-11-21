\name{rockpools}
\alias{rockpools}
\docType{data}
\title{
Rockpool Data
}
\description{
A data set with pH measurements in rockpools that received different
treatments.
}
\usage{data(rockpools)}
\format{
  A data frame with 58 observations on the following 4 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{time}}{a factor with levels \code{beginning} \code{end}}
    \item{\code{treatment}}{a factor with levels \code{control} \code{duckshit}}
    \item{\code{pH}}{a numeric vector}
  }
}
\details{
none
}
\source{
from Dieter Ebert with some manipulations for didactic reasons
}
\references{
none  
}
\examples{
data(rockpools)
str(rockpools)
}
\keyword{datasets}
