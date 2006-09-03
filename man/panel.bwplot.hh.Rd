\name{panel.bwplot.hh}
\alias{panel.bwplot.hh}
\alias{panel.bwplot.intermediate.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel functions for bwplot.}
\description{
  Panel functions for \code{bwplot} that give the user
  control over the placement of the boxes.
}
\usage{
panel.bwplot.hh(x, y, box.ratio = 1, horizontal = TRUE, pch = box.dot$pch, 
    col = box.dot$col, cex = box.dot$cex, fill = box.rectangle$fill, 
    varwidth = FALSE, levels.fos = NULL, coef = 1.5, at, ...)

panel.bwplot.intermediate.hh(x, y, box.ratio = 1, horizontal = TRUE,
    pch = box.dot$pch,  col = box.dot$col, cex = box.dot$cex,
    fill = box.rectangle$fill, levels.fos = NULL, at, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, pch, col, cex, fill, varwidth, horizontal,
    levels.fos, box.ratio} {see
    \code{\link[lattice]{bwplot} and \code{\link[lattice]{panel.bwplot}}}}
  \item{coef}{see \link[grDevices]{boxplot.stats}}
  \item{at}{ \code{x} location of the individual boxes. }
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
