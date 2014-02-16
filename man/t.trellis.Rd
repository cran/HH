\name{t.trellis}
\alias{t.trellis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interchange the x- and y-axes for an S-Plus trellis object.
  Interchange the conditioning variables for an R trellis object.}
\description{
  In S-Plus, change the \code{"trellis"} object to effectively, and
  after-the-fact convert formulas\cr
  from \code{(y ~ x | g)} to \code{(x ~ y | g)}.
  This is needed in S-Plus because most S-Plus trellis
  functions do not permit factors on the right-hand side of the formula.

  In R lattice functions, factors are permitted on the right-hand side
  of the formula.  Therefore, to change the x and y axes within each
  panel, change the formula\cr
  from \code{(y ~ x | g)} to \code{(x ~ y | g)}.  The HH \code{t.trellis} function in R calls
  \code{lattice::t.trellis} to interchange the conditioning variables
  for an R trellis object.  See \code{\link[lattice]{update.trellis}}.

}
\usage{
\method{t}{trellis}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{any \code{"trellis"} object.}
}
\details{
  In S-Plus, a warning is generated if the panel component of the
  \code{"trellis"} object is a function.  No warning is generated when
  the panel component of the \code{"trellis"} object is a character
  string naming the function.  We interchange the \code{x} and \code{y}
  values of the\cr
  \code{c("x", "y", "xlab", "ylab", "xlim", "ylim", "adj.xlim", "adj.ylim")}
  components of the \code{"trellis"} object,
  and are aware of special features of the panel functions listed in the
  S-Plus version of \code{t.trellis}.  The transpose will usually work
  correctly for other panel functions.
}
\value{
In S-Plus, a \code{"trellis"} object with all x and y components
interchanged.
In R, the argument \code{"trellis"} object is sent to \code{lattice::t.trellis}
to interchange the conditioning variables.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
  \code{\link[lattice]{update.trellis}}
#endif
#ifdef S-Plus
  \code{\link[trellis]{xyplot}}
#endif
}
\examples{
tmp <- data.frame(y=rnorm(30), x=factor(rep(1:3,10)))
if.R(r=
     bwplot(y ~ x, data=tmp)
     ,s=
     t(bwplot(x ~ y, data=tmp))
     )
}
\keyword{dplot }
