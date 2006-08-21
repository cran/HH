\name{strip.xysplom}
\alias{strip.xysplom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{strip function that is able to place the correlation or regression
  coefficient into the strip label.}
\description{
  strip function that is able to place the correlation and/or regression
  coefficient into the strip label.
}
\usage{
strip.xysplom(which.given, which.panel, var.name, factor.levels,
 shingle.intervals, par.strip.text = trellis.par.get("add.text"),
 strip.names = c(TRUE, TRUE), style = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which.given, which.panel, var.name, factor.levels,
    shingle.intervals}{arguments to \code{strip.default}.}

  \item{par.strip.text, strip.names, style, \dots}{more arguments to
    \code{strip.default}.}
}
\details{
  The function looks for the specific factor names
  \code{c("corr","beta","corr.beta")}.  If it finds them, it goes up the
  calling sequence to locate the data for the panel.  Then it calculates
  the correlation and/of regression coefficient and inserts the
  calculated value(s) as the value for the strip label.  
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{xysplom}}}
\keyword{dplot }
