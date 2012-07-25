\name{panel.ci.plot}
\alias{panel.ci.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Default Panel Function for ci.plot }
\description{
  This is the default panel function for \code{ci.plot}.
}
\usage{
panel.ci.plot(x, y, newdata, newfit = newfit, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Observed values of predictor variable.}
  \item{y}{Observed values of response variable.}
  \item{newdata}{\code{x} values for which predictions are calculated.}
  \item{newfit}{\code{data.frame} containing six components:
    \code{"fit"}, \code{"se.fit"}, \code{"residual.scale"}, \code{"df"},
    \code{"ci.fit"}, \code{"pi.fit"}.  In S-Plus these are the output from the
    \code{predict.lm} function.  In R they are a rearrangement of the
    output of the \code{predict.lm} function.}
  \item{\dots}{  other arguments passed to \code{panel.xyplot}. }
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{ci.plot}}, \code{\link[lattice]{xyplot}}, \code{\link[stats]{lm}}}
\keyword{ dplot }
