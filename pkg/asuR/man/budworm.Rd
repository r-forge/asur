\name{budworm}
\alias{budworm}
\docType{data}
\title{budworm data}
\description{Data from a small experiment on the toxicity to the tobacco budworm
}
\usage{data(budworm)}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{num.dead}}{a numeric vector}
    \item{\code{num.alive}}{a numeric vector}
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{dose}}{a numeric vector}
  }
}
\details{
  Experiment on the toxicity to the tobacco budworm Heliothis
  virescens of doses of the pyrethoid trans-cypermethrin to which the
  moths were beginning to show resistance. Batches of 20 moths of each
  sex were exposed for three days to the pyrethroid and the number in
  each batch that were dead or knocked down was recorded.
}
\source{
Collette(1991)
}
\references{
from MASS p.190; by Venables and Ripley (see also there for suggestions
on how to analyse)
}
\examples{
% ## construced with:
% num.dead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
% num.alive <- 20-num.dead
% sex <- factor(rep(c("male", "female"), c(6,6)))
% dose <- c(1,2,4,8,16,32)
% budworm <- data.frame(num.dead=num.dead, num.alive=num.alive, sex=sex, dose=dose)
## usage:
data(budworm)

budworm.contr <- rbind("female-male"=c(1,-1))
b.glm <- glm(cbind(num.dead,num.alive) ~ sex*log2(dose),
               family=binomial,
               contrasts=list(sex=mycontr(contr=budworm.contr)),
               data=budworm)

b1.glm <- glm(cbind(num.dead,num.alive) ~ sex*I(log2(dose)-3),
               family=binomial,
               contrasts=list(sex=mycontr(contr=budworm.contr)),
               data=budworm)

b2.glm <- glm(cbind(num.dead,num.alive) ~ sex + I(log2(dose)-3),
               family=binomial,
               contrasts=list(sex=mycontr(contr=budworm.contr)),
               data=budworm)
}

% plot(x=c(1,32), y=c(0,1), type="n", xlab="dose", ylab="prob", log="x")
% text(2^log.dose,numdeae/20, labels=as.charecter(sex))

\keyword{datasets}
