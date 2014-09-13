\name{cplx}
\alias{cplx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a sequence spanning the xlim of a lattice window.
}
\description{
Generate a sequence of \code{length} points spanning the
\code{current.panel.limits()$xlim} of a lattice window.
}
\usage{
cplx(length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{length}{
Integer number of points.
}
}
\value{
  One-column matrix containing \code{length} rows.
  The first value is the x-value at the left side of the window.  The
  last value is the x-value at the right side of the window.
  The in between points are evenly spaced.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[base]{scale}}
}
\examples{
cplx(11)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
