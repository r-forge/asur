\name{flowers}
\alias{flowers}
\docType{data}
\title{Flower}
\description{
A data set with the dry mass of all flowers and the dry mass of the
total plant from 20 species growing at high and 20  species growing at low altitude.
}
\usage{data(flowers)}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{alt}}{a factor with levels \code{high} \code{low}}
    \item{\code{flower}}{a numeric vector; dry mass of flowers in mg}
    \item{\code{total}}{a numeric vector; dry mass of total plant in mg}
  }
}
\details{
  The data sets shows a random subsample of the original data set.
}
\source{
Fabbro, T. & Koerner, Ch. (2004): \emph{Altitudinal differences in
  flower traits and reproductive allocation.} FLORA 199, 70-81.
  Fabbro, Koerner ()
}
\examples{
data(flowers)
% ## having a look at the data
% #  xyplot(log(flower) ~ log(total)|alt, data=flowers,
% #           panel=function(x,y,...){
% #           panel.xyplot(x,y,...)
% #           panel.lmline(x,y,...)
% #         })
## a model with two intercepts and two slopes
#  m1 <-lm(log(flower) ~ alt/log(total) -1, data=flowers)
#
## a model with two intercepts and one slope
#  altdiff <- rbind("high-low"=c(1,-1))
#  m2 <- lm(log(flower) ~ alt + log(total), data=flowers,
#           contrasts=list(alt=mycontr(contr=altdiff)))
#
## are separate slopes needed?
#  anova(m1, m2) # conclusion:
#
## no difference in slopes but difference in intercept
## for interpretation also test whether the slope is one!
}
\keyword{datasets}
