\name{plants}
\alias{plants}
\docType{data}
\title{Plant height}
\description{
The height of all species of Fabaceae and Rosaceae growing in
Switzerland. As well as a factor indicating whether the species can grow
to a shrub, tree, or stays herbaceous.
}
\usage{data(plants)}
\format{
  A data frame with 460 observations on the following 3 variables.
  \describe{
    \item{\code{family}}{a factor with levels \code{Fabaceae} \code{Rosaceae}}
    \item{\code{height}}{a numeric vector}
    \item{\code{type}}{a factor with levels \code{herb} \code{shrub} \code{tree}}
  }
}
\source{
thomas.fabbro@unibas.ch
}
\examples{
data(plants)
tapply(plants$height, list(plants$family, plants$type), mean)
tapply(plants$height, list(plants$family, plants$type), summary)

tapply(plants$height, list(plants$family, plants$type), length)

table(plants$family, plants$type)
}
\keyword{datasets}
