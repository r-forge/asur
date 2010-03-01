\name{oring}
\alias{oring}
\docType{data}
\title{O-ring data}
\description{
Data of O-ring failures in different tests
}
\usage{data(oring)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{temperature}}{a numeric vector}
    \item{\code{failures}}{a numeric vector}
    \item{\code{fail}}{a factor with levels \code{no} \code{yes}}
  }
}
\details{
  Space shuttle Challenger exploded right at the beginning of its flight on January 28, 1986. This was one of the largest disasters in the American space program. The night before, an engineer had recommended to NASA (National Aeronautics and Space Administration) that the shuttle should not be launched in the cold weather. Forecast of temperature for the launch was 31 degrees Fahrenheit, the coldest launch ever. This suggestion was over-ruled. Inquiry Commission appointed by the President of the United States, wanted to see if enough evidence existed to predict serious trouble due to low temperature at the time of launch. Since the shuttle had, up to that time, not met with any accident, the only evidence available was regarding damage to O-rings. These rubber rings fill the gaps between parts of the giant tube that makes the rocket. If there is even a minor leak, hot gases push through it and in milliseconds, large portion of the rocket fuel can come out to destroy the rocket. Hence damaged O-rings can be treated as signs of major trouble. Such instances had indeed been recorded in previous flights of the shuttle. Data are to be analyzed to check if statistical methods would have given the right guidance.
}
\source{
from http://wps.aw.com/wps/media/objects/15/15719/projects/, without additional information about the source
}
\examples{
% ### generated as:
% #oring <- read.table("~/temp/oring.txt", sep="\t", header=TRUE)
% #oring$number <- NULL
% #oring$date <- as.Date(oring$date, "%m/%d/%y")
% #oring$failures[4] <- NA
% #oring$failures <- as.numeric(oring$failures)-2
% #oring$fail <- factor(ifelse(oring$failures>0,"yes","no"))

data(oring)
model.glm <- glm(fail ~ temperature, data=oring, family=binomial)
}
\keyword{datasets}
