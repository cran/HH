\name{push.vp.hh}
\alias{push.vp.hh}
\alias{pop.vp.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{push and pop a grid viewport, turn clipping off, change scale.}
\description{
  push and pop a grid viewport, turn clipping off, change scale.
}
\usage{
push.vp.hh(scale = 100)
pop.vp.hh()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scale}{argument to the \code{\link[grid]{unit}} function.}
}
\details{
  Used in \code{\link{panel.cartesian}} to ease labeling the rows
  and columns of a scatterplot matrix.
}
\value{
  An object of class \code{"unit"}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{ \code{\link[grid]{viewport}}, \code{\link[grid]{unit}},
  \code{\link{panel.cartesian}}}
\keyword{dplot }
