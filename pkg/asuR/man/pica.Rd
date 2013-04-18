\name{pica}
\alias{pica}
\docType{data}
\title{Numnber of birds and presence of Pica pica from Agrovian monitoring scheme}
\description{Data from monitoring scheme of Agrovia.}
\usage{data(pica)}
\format{
  A data frame with 516 observations on the following 5 variables.
  \describe{
    \item{\code{pica}}{Presence and absence of Pica pica (Elster)}
    \item{\code{SR}}{Number of species (species richness)}
    \item{\code{land_use}}{a factor with levels 
          \code{keine_Hauptnutzung}, 
          \code{Landwirtschaft}, 
          \code{Siedlung}, and  
          \code{Wald}}
    \item{\code{altitude}}{meter above sea level}
    \item{\code{BGR}}{a factor with levels 
          \code{Flusstal} 
          \code{Huegelland} 
          \code{Jura}}
  }
}
\examples{
data(pica)
head(pica)
}
\keyword{dataset}
