\name{photinus}
\alias{photinus}
\docType{data}
\title{
  Spermatophore number and its influence on fitness in Photinus ignitus
}
\description{
How does the nuptial gifts that males provide to females in the form of
spermatophores, sperm-containing structurs, influence the numer of eggs.
}
\usage{data(photinus)}
\format{
  A data frame with 42 observations on the following 3 variables.
  \describe{
    \item{\code{eggs}}{a numeric vector, number of eggs a female laid}
    \item{\code{weight}}{a numeric vector, wet weight of females}
    \item{\code{number}}{number of spermatophores a female received, a factor with levels \code{single} \code{tripple}}
  }
}
\source{
From a picture in Ecol. Ento. 27, 373-377. The data given here was just
read by eyes and probably some points are missing.
}
\examples{
data(photinus)
str(photinus)
}
\keyword{datasets}
