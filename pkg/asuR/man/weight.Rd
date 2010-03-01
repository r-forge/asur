\name{weight}
\alias{weight}
\docType{data}
\title{Weight Gain}
\description{
The data arise from an experiment to study the gain in weight of
     rats fed on four different diets, distinguished by amount of
     protein (low and high) and by source of protein (beef and cereal).
}
\usage{data(weight)}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{source}}{a factor with levels \code{Beef} \code{Cereal};
    source of the protein fed}
    \item{\code{type}}{a factor with levels \code{High} \code{Low},
      amount of the protein fed}
    \item{\code{weightgain}}{a numeric vector; weight gained in grams}
  }
}
\details{
Ten rats are randomized to each of the four treatments. The
     question of interest is how diet affects weight gain.
}
\source{
D. J. Hand, F. Daly, A. D. Lunn, K. J. McConway and E. Ostrowski
(1994). A Handbook of Small Datasets, Chapman and Hall/CRC,
London.
}
\references{
from library(HSAUR)
}
\examples{
data(weight)
interaction.plot(weight$type, weight$source, 
                        weight$weightgain)
}
\keyword{datasets}
