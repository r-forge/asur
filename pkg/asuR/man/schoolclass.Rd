\name{schoolclass}
\alias{schoolclass}
\docType{data}
\title{Schools and Classes}
\description{
A data set for studying nested random factors.
}
\usage{data(schoolclass)}
\format{
  A data frame with 1000 observations on the following 4 variables.
  \describe{
    \item{\code{school}}{a factor with 20 levels}
    \item{\code{class}}{a factor with 5 levels}
    \item{\code{gender}}{a factor with levels \code{female} \code{male}}
    \item{\code{score}}{a numeric vector}
  }
}
\details{
A simulated data set with 20 schools, each with 5 classes with 10 kids. The parameters: variance among schools (100),
variance among classes (50), residual variance (10), factor gender:
level female (120), level male (80).
}
\source{
\email{thomas.fabbro@unibas.ch}
}
\examples{
data(schoolclass)
str(schoolclass)
#
# with library(lme4)
# (m0 <- lmer(score ~ gender + (1|school/class), data=schoolclass))
}
\keyword{datasets}
% #############################################################################
% ## generating the data set schoolclass.rda
% #############################################################################
% ## schools
% school.nr=20; school.var=100
% ## each 5 classes ->  100 classes
% class.nr=5; class.var=50
% ## each 10 kids   -> 1000 observations
% kids.nr=10; kids.var=10
% gender.means=c(80,120)
% #############################################################################
% school.realized <- rnorm(school.nr, mean=0, sd=sqrt(school.var))
% class.realized <- rnorm(school.nr*class.nr, mean=0, sd=sqrt(class.var))
% kids.realized <- rnorm(school.nr*class.nr*kids.nr, mean=0, sd=sqrt(kids.var))
% ###
% schoolclass <- data.frame(#
%                           gender=rep(c("male","female"), each=(school.nr*class.nr*kids.nr)/2),#
%                           school=rep(paste("s_",1:school.nr,sep=""),each=class.nr*kids.nr),#
%                           class=rep(paste("c_",1:(school.nr*class.nr),sep=""),each=kids.nr),#
%                           score=rep(school.realized, each=class.nr*kids.nr)+rep(class.realized, each=kids.nr)+kids.realized+rep(gender.means, each=(school.nr*class.nr*kids.nr)/2)
%                      )
% #############################################################################
% save(schoolclass, file="~/checkouts/asuR.svn/asuR/data/schoolclass.rda")
% #############################################################################
% ### testing
% m1 <- lmer(score ~ gender + (1|school/class), data=schoolclass)
% #############################################################################