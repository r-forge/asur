\name{growth}
\alias{growth}
\docType{data}
\title{Weight Gain of two Species at different Nitrogen Concentrations}
\description{
For 30 individuals of species A and B the weight gain during one week
and the soil nitrogen concentration was measured. The aim of the study
was to test whether this two species respond differently.
}
\usage{data(growth)}
\format{
  A data frame with 60 observations on the following 3 variables.
  \describe{
    \item{\code{gain}}{a numeric vector; weight gain in mg}
    \item{\code{species}}{a factor with levels \code{sp_A} \code{sp_B}}
    \item{\code{nitrogen}}{a numeric vector; the soil nitrogen concentration}
  }
}
\source{
\email{thomas.fabbro@unibas.ch}
}
\examples{
data(growth)
}
\keyword{datasets}
