\name{norm.test}
\alias{norm.test}
\alias{norm}
\title{Inspect your residuals after transformation}
\description{
This function shows you nine normal quantile-quantile plots. Eight are
sampled from a normal distribution and one is showing your data. Do you
recognise your data?
}
\usage{
norm.test(x)
}
\arguments{
  \item{x}{a numeric vector with your data}
}
\value{
Nine quantile-quantile plots on your active device.
}
\author{\email{thomas.fabbro@unibas.ch}}
\examples{
#norm.test(rchisq(30,df=10))
}
\keyword{file}
