\name{multicomp.reverse}
\alias{multicomp.reverse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Force all comparisons in a "multicomp" object to have the same sign.}
\description{
  Force all comparisons in a \code{"multicomp"} object
  to have the same sign.  If the contrast \code{"A-B"} has a negative estimate,
  reverse it show the contrast \code{"B-A"} with a positive estimate.
}
\usage{
multicomp.reverse(y, estimate.sign = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{\code{"multicomp"} object}
  \item{estimate.sign}{If \code{estimate.sign==1}, reverse the negatives.
    If \code{estimate.sign==-1}, reverse the positives.
    Both the names of the comparisons and the numerical values are reversed.
    If \code{estimate.sign==0}, return the argument.}
  \item{\dots}{other arguments not used. }
}
\value{
The result is a \code{"multicomp"} object containing the same contrasts
as the argument but with the sign of the contrasts changed as needed.
}
\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
  S-Plus use the \code{multicomp} functions and R uses the \code{multcomp}
  package.
}
\seealso{\code{\link{MMC}}, \code{\link{multicomp.order}}}
\examples{
## see example in multicomp.order
}
\keyword{ dplot }
