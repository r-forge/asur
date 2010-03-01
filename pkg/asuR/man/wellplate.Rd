\name{wellplate}
\alias{wellplate}
\alias{wellplate2}
\alias{wellplate3}
\docType{data}
\title{wellplate}
\description{
An example data set with two random variables that are crossed (row and
column). And one fixed variable a treatment.
}
\usage{data(wellplate)}
\format{
  A data frame with 96 observations on the following 4 variables.
  \describe{
    \item{\code{row}}{a factor with 8 levels}
    \item{\code{col}}{a factor with 12 levels}
    \item{\code{trt}}{a factor with levels \code{contr} \code{treat}}
    \item{\code{int}}{a numeric vector}
  }
}
\details{
A simulated data set with the parameters: variance among rows (50),
variance among columns (150), residual variance (10),
factor trt: levels: "contr" (100), "treat" (200).
\cr
\cr
\strong{DATASETS} with different column variance structure:\cr
wellplate2: variance among columns ("treat" 150; "contr" 0; covariance 0)\cr
wellplate3: variance among columns ("treat" 150; "contr" 150; covariance 0)
}
\source{
simulated by \email{thomas.fabbro@unibas.ch}
}
\examples{
data(wellplate)
str(wellplate)
%
% Imagine you have a wellplate where you used a multi-channel pipette to
% add a coloured substance column by column and a multi-channel pipette to add a
% coloured substance row by row. You know that it will always add the same
% amount to all tips but that there will be changes between different
% steps of pipetting.
%
% 1) How can you estimate how variable the pipettes are?
# library(lme4)
# (model <- lmer(int ~ 1 + (1|row) + (1|col), data=wellplate))
%
% If you have a treatment within the wells of your plate you can add
% simply a treatment factor.
%
#(model <- lmer(int ~ 1 + (1|row) + (1|col), data=wellplate))
%
% Imagine now that the substance you add column by column is a staining
% that has itself a colour and stains especially the treated wells. You
% want to estimate how large the variance is that you get from the
% column-wise pipetting; for both levels of the treatment separately. 
%
# (model2 <- lmer(int ~ trt + (1|row) + (1|trt:col), data=wellplate2))
%
% You can now imagine that the column-wise staining effects all well in a
% column because you dip with your pipette into the liquid an mix some
% part of it. Therefore you also need to estimate the covariance between
% the two column variance components.
%
# (model3 <- lmer(int ~ trt + (1|row) + (trt-1|col), data=wellplate3))
}
\keyword{datasets}
