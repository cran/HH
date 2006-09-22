\name{panel.bwplot.intermediate.hh}
\alias{panel.bwplot.intermediate.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel functions for bwplot.}
\description{
  Panel function for \code{bwplot} that give the user
  control over the placement of the boxes.
}
\usage{
panel.bwplot.intermediate.hh(x, y, horizontal = TRUE,
    pch = box.dot$pch,  col = box.dot$col, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, pch, col, horizontal} {see
    \code{\link[lattice]{bwplot} and \code{\link[lattice]{panel.bwplot}}}}
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
  \code{\link[lattice]{bwplot}}, \code{\link{interaction2wt}}}
\keyword{dplot }
