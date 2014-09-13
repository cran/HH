\name{latticeresids}
\alias{latticeresids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subroutine used by residual.plots.lattice
}
\description{
Subroutine used by \code{residual.plots.lattice}
}
\usage{
latticeresids(x, data,
              main = "please use an appropriate main title",
              par.strip.text, scales.cex, y.relation, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, data, main, par.strip.text, \dots}{
\pkg{lattice} arguments.  See \code{\link{xyplot}}.
}
  \item{scales.cex}{
\code{cex} for the \code{scales} argument in \code{\link{xyplot}}.
}
  \item{y.relation}{
\code{relation} for the \code{y} argument to \code{scales} argument in \code{\link{xyplot}}.
}
}
\value{
\code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{residual.plots.lattice}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
