\name{panel.bwplot.intermediate.hh}
\alias{panel.bwplot.intermediate.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel functions for bwplot.}
\description{
  Panel function for \code{bwplot} that give the user
  control over the placement of the boxes.  When used with a positioned
  factor, the boxes are placed according to the position associated with
  the factor.
}
\usage{
panel.bwplot.intermediate.hh(x, y, horizontal = TRUE,
          transpose=!horizontal,
          pch, col,
          at,  ## formerly S-Plus only, now totally ignored
          ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, pch, col, horizontal}{see
    \code{\link[lattice]{xyplot}} and
      \code{\link[lattice]{panel.bwplot}}.}

  \item{transpose}{S-Plus only.  The HH library transposes
    \code{"trellis"} \code{bwplot} objects to put the response variable
    on the vertical axis.  R does the equivalent by placing the response
    variable on the left side of the \code{"~"} in the formula and with
    the \code{horizontal} argument.}

  \item{at}{formerly S-Plus only, now totally ignored.}
  \item{\dots}{ Extra arguments, if any, for 'panel.bwplot'.  }
}
\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[lattice]{panel.xyplot}},
  \code{\link[lattice]{xyplot}}, \code{\link{interaction2wt}},
  \code{\link{position}}}
\keyword{dplot }
