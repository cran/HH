\name{panel.xysplom}
\alias{panel.xysplom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{panel method for xysplom.}
\description{
 panel method for xysplom.  It has a \code{corr} argument that is
 removed
 before sending the information on to panel.xyplot.
}
\usage{
panel.xysplom(corr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corr}{logical.  If \code{TRUE}, display the correlation and/or
    the regression
    coefficient for \code{lm(y ~ x)} for each panel in an additional
    strip label.}
  \item{\dots}{Remaining arguments to \code{panel.xyplot}.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{xysplom}}}
\keyword{dplot }
