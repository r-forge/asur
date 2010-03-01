\name{cathedral}
\alias{cathedral}
\docType{data}
\title{Medieval cathedrals in England}
\description{
The length an height of medieval cathederals in England and whether they
are belonging to the Gotic or Romanesque style. Some cathedrals have parts in both styles and are listed twice.
}
\usage{data(cathedral)}
\format{
  A data frame with 25 observations on the following 3 variables.
  \describe{
    \item{\code{style}}{a factor with levels \code{g}, Gothic, \code{r},
    Romanesque}
    \item{\code{x}}{a numeric vector, nave height}
    \item{\code{y}}{a numeric vector, total length in feet}
  }
}
\source{
From Julian Faraway
}
\examples{
data(cathedral)
}
\keyword{datasets}
