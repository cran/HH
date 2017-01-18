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
          pch, col, lwd,
          ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, pch, col, lwd, horizontal}{see
    \code{\link[lattice]{xyplot}} and
      \code{\link[lattice]{panel.bwplot}}.}

  \item{\dots}{ Extra arguments, if any, for 'panel.bwplot'.  }
}
\examples{
## see examples at
\dontrun{
  demo("bwplot.examples", package="HH")
}
}

\references{
  Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in R}, Second Edition.
 Springer Texts in Statistics. Springer.
 ISBN 978-1-4939-2121-8.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[lattice]{panel.xyplot}},
  \code{\link[lattice]{xyplot}}, \code{\link{interaction2wt}},
  \code{\link{position}}}
\keyword{dplot }
