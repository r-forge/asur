\name{inspect}
\alias{inspect}
\alias{inspect.lm}
\alias{inspect,lm-method}
\alias{inspect,glm-method}
\alias{[,Formula-method}
\alias{[-method}
\alias{[,pDescList-method}
\alias{[<-,pDescList-method}
\alias{dep}
\alias{hnp}
\alias{hnp,glm-method}
\alias{ihp}
\alias{ilp}
\alias{irp}
\alias{rpp}
\alias{rpp,lm-method}
\alias{lep}
\alias{lep,glm-method}
\alias{prp}
\alias{ryp}
\title{Plots to inspect fitted models}
\description{This functions can be used to validate some assumptions of linear
regressions, generalized linear regressions and probably more.
The idea of this functions is to illustrate possibilities,
not to provide fully functional code. The functions work for
regression models with continous variables only.}
\usage{
inspect(mymodel, which = c("select", "sequence", "all"), id = c("all", "none"), ...)
dep(mymodel, id= c("all", "none"), ...)
hnp(mymodel, id= c("all", "none"), ...)
ihp(mymodel, id= c("all", "none"), ...)
ilp(mymodel, id= c("all", "none"), ...)
irp(mymodel, id= c("all", "none"), ...)
lep(mymodel, id= c("all", "none"), ...)
prp(mymodel, id= c("all", "none"), ...)
ryp(mymodel, id= c("all", "none"), ...)
rpp(mymodel, id= c("all", "none"), ...)
}
\arguments{
  \item{mymodel}{an object of class \code{glm} or \code{lm}, usually, a result of a
    call to the function \code{glm} or \code{lm}}
  \item{which}{a character string defining how the inspection plots for
    the class of mymodel are presented; one of the following:
    \code{select}
    select a diagnostic plot from a menu with all plots available
    (also repeatedly selecting the same plot is possible)
    \code{sequence}
    plot all available diagnostic plots in a sequence
    (with the possibility to skip a plot or quit)
    \code{all}
    plot all available diagnositc plots without user interaction
    (mainly useful for printing, see examples)
  }
  \item{id}{a character string or numeric value; in which panel should
    it be possible to interactively identify values}
  \item{...}{further arguments}
}
\section{Methods}{
\describe{

\item{\code{signature(mymodel = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(mymodel = "glm")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(mymodel = "lm")}}{
%%  ~~describe this method here~~
}

}}
\details{The function inspect calls other functions (depending on their
  class) to inspect the model assumptions.
}
\value{
A list with identified values (row names), one slot for each inspection function.
}
\author{\email{thomas.fabbro@unibas.ch}}
\note{On some devices you are not allowed to resize the window before
  interactively selecting points}
\seealso{\code{plot}}
\examples{
data(swiss)
model <-  lm(Fertility ~ ., data = swiss)
\dontrun{i.lm <- inspect(model, which = "sequence")}
\dontrun{model2 <- lm(Fertility ~ ., data = swiss[-i.lm,]}
\dontshow{i.lm <- inspect(model, which = "all", id = "none")}
\dontrun{i.lm <- inspect(model, which = "all", id = "none")}


### for printing
\dontrun{
pdf(~/temp/diagnostic.pdf)
inspect(model)
dev.off()
}
}
\keyword{file}
