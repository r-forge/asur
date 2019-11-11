\name{tribolium}
\alias{tribolium}
\docType{data}
\title{Tribolium data}
\description{
The data come from a study of water loss in Tribolium confusum, the
confused flour beetle. Nine batches of 10 beetles were weighted, kept at
different relative humidities, and weighed again after six days of
starvation. Weight loss in milligrams was computed.
}
\usage{data(tribolium)}
\format{
  A data frame with 81 observations on the following 2 variables.
  \describe{
    \item{\code{humidity}}{a numeric vector, percent relative humidity}
    \item{\code{weight.loss}}{a numeric vector, in milligram}
  }
}
\source{
  Data set was created based on the Table 14.1 from Sokal, R. R. & Rohlf, J. F. (1995): \emph{Biometry: the principles and practice of 
    statistics in biological research.} Freeman, New York (p 459).
}
\examples{
%% created as:
%% tribolium <- data.frame(humidity = rep(c(0, 12, 29.5, 43, 53, 62.5, 75.5, 85, 93), each = 9),
%% ##            weight.loss = as.vector(sapply(c(8.98, 8.14, 6.67, 6.08, 5.9, 5.83, 4.68, 4.2, 3.72), function(x){rnorm(9, x, 2)})))
data(tribolium)
}
\keyword{datasets}
