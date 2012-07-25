\name{panel.bwplott}
\alias{panel.bwplott}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extension to S-Plus trellis to allow transposed plots.}
\description{
  Extension to S-Plus trellis to allow transposed plots.  All
  x - and y-components of the trellis object are interchanged.
  This function is not needed in R as lattice has a horizontal argument
  in its definitions.}
\usage{
panel.bwplott(x, y, box.ratio = 1,
              font = box.dot$font, pch = box.dot$pch, cex = box.dot$cex,
              col = box.dot$col, ..., transpose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, box.ratio, font, pch, cex, col, \dots}{See
%% R
#ifndef S-Plus
    \code{\link[lattice]{panel.bwplot}}.
#endif
#ifdef S-Plus
    \code{\link[trellis]{panel.bwplot}}.
#endif
  }
  \item{transpose}{logical.  If \code{FALSE}, the plot is printed in the
    default orientation.  If \code{TRUE}, the \code{x}- and
    \code{y}-components
    of the trellis object are interchanged.  This has the effect, for
    example,
    of displaying vertical boxplots instead of the default horizontal boxplots.
  }
}
\value{
The function is used for its side effect of drawing boxplots in a
trellis panel.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
This function is not needed in R.  If it is used and 
}
\seealso{
%% R
#ifndef S-Plus
  \code{\link[lattice]{xyplot}}.
#endif
#ifdef S-Plus
  \code{\link[trellis]{bwplot}}.
#endif
}
\keyword{ dplot }
