\name{print.latticeresids}
\alias{print.latticeresids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a \code{latticeresids} object.
}
\description{
Print a \code{latticeresids} object.
}
\usage{
\method{print}{latticeresids}(x, ...,
           A321.left=0, A321.bottom=0.27,
           A4.left=0, A4.top=0.30,
           position=list(
             A321=c(A321.left,     A321.bottom, 1, 1     ),
             A4  =c(A4.left,       0,           1, A4.top)),
           panel.width=NULL,
           which=1:4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A \code{latticeresids} object.
  }
  \item{A321.left, A321.bottom, A4.left, A4.top, position}{
    The first three rows are on the same \code{x} scale (the scales of
    the independent variables).  The arguments
    with \code{"A321"} in their name are used to construct the
    \code{position} argument to \code{\link[lattice]{print.trellis}} for
    the first three rows.  The fourth row is on a different \code{x}
    scale (the scales of each independent variable adjusted for all the
    other \code{x} variables).
      The arguments
    with \code{"A4"} in their name are used to construct the
    \code{position} argument to \code{\link[lattice]{print.trellis}} for
    the fourth row.  The two sets of rows \{1,2,3\} and \{4\} may have
    different
    widths for their left axis tick labels.  The arguments
    \code{A321.left} and \code{A4.left} along with absolute dimensions
    for \code{panel.width} (\code{"cm"} or \code{"in"}, not \code{"npc"})
    can be hand-tailored to make the columns line up precisely.  See the example.
  }
  \item{panel.width}{the \code{panel.width} argument of
    \code{\link[lattice]{print.trellis}}.
  }
  \item{which}{Vector of row numbers which are to be printed.  If not
    all four printed, consider adjusting the \code{A321.bottom} and
    \code{A4.top} values.
  }
  \item{\dots}{
    Other arguments for \code{print}.
  }
}
\details{
  The four trellis objects, one for each type of plot, are printed as
  a single four-row lattice object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{residual.plots.lattice}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
