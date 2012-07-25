\name{extra}
#ifndef S-Plus
\alias{as.rts}
#endif
\alias{units.ts}
\alias{title.trellis}
\alias{title.grob}
\alias{as.character.arima.model}
\alias{arima.model}
\alias{coefArimaHH}
\alias{.arima.info.names.not.ordered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Miscellaneous functions that I wish were in or consistent between
S-Plus and R.}
\description{
  Miscellaneous functions that I wish were in or consistent between
S-Plus and R.
}
\usage{
#ifndef S-Plus
as.rts(x, ...)
#endif

\method{units}{ts}(x)

title.trellis(main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    line = NA, outer = FALSE, axes=NULL, ...)

title.grob(main=NULL, y=.985, gp=gpar(cex=1.5))

\method{as.character}{arima.model}(x, ...)

arima.model(x)

coefArimaHH(object, ...)

.arima.info.names.not.ordered (model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector or time series}
  \item{\dots}{Additional arguments.}
  \item{main, sub, xlab, ylab, line, outer, axes}{See \code{title}.}
  \item{model}{A time series model specification in the S-Plus notation.}
  \item{object}{\code{"arima"} object in S-Plus.}
  \item{y, gp}{See \code{\link[grid]{grid.text}} in R.}
}
\value{ The result object of \code{arima.model} has class \code{"arima.model"}}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{arma.loop}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ hplot }
