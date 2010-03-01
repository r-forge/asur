\name{unemployment}
\alias{unemployment}
\docType{data}
\title{data on long / short term unemployment}
\description{
Data set with success (long term) unemployment against total number o unemployment.
}
\usage{data(unemployment)}
\format{
  A data frame with 983 observations on the following 2 variables.
  \describe{
    \item{\code{success}}{a factor with levels \code{<6m} \code{>6m}}
    \item{\code{gender}}{a factor with levels \code{female} \code{male}}
  }
}
\details{
no
}
\source{
GLM course notes by Gerhard Tutz (have a look at his nice book!)
}
\examples{
data(unemployment)
contrast.matrix <- rbind("male-female"=c(1,-1))

u.glm <- glm(success~gender, data=unemployment, family=binomial, contrasts=list(gender=mycontr(contrast.matrix)))

###  for data in the grouped form
#################################
### we make a data set in grouped form
table(unemployment$gender, unemployment$success)
unemployment.grouped <- data.frame(longterm=c(167,175), shortterm=c(403,238), gender=c("male","female"))

u.glm.grouped <- glm(cbind(longterm,shortterm) ~ gender, data=unemployment.grouped, family=binomial, contrasts=list(gender=mycontr(contrast.matrix)))
summary(u.glm.grouped)
coefficients(u.glm.grouped)

### extracting the factors by which the odds change from male to female
factor <- exp(coefficients(u.glm.grouped)[2])

### the "same" calculations by hand:
attach(unemployment.grouped)
p <- longterm/(shortterm+longterm)
detach(unemployment.grouped)
odds <- p/(1-p)

odds[1]*factor
### should give the same as
odds[2]

###   for those who like p-values
#################################
anova(u.glm, test="Chisq")
### is the same
anova(u.glm.grouped,test="Chisq")
### the same p-value
pchisq(17.959,1,lower.tail=FALSE)
### and also almost the same p-value as with a chisq.test()...that you
### can try yourself!
% the data was generated using :
% unemployment <- data.frame(success=factor(rep(c(">6m","<6m"),c(167+175,403+238))), gender=factor(rep(c("male","female","male","female"),c(167,175,403,238)))) 
}
\keyword{datasets}
