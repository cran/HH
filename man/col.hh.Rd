\name{col.hh}
\alias{col.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initializing Trellis Displays}
\description{
     Initialization of a display device with the graphical
     parameters that rmh prefers.
}
\usage{
col.hh()
}
%- maybe also 'usage' for other objects documented here.
\value{
  List of graphical parameters to be used in the \code{theme} argument
  to the \code{trellis.device} or \code{trellis.par.set} functions.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[lattice]{trellis.device}}, \code{\link[lattice]{trellis.par.get}}}
\examples{
## function name or result of function call
trellis.device(theme="col.hh")
trellis.device(theme=col.hh())

## result of function call
trellis.par.set(theme=col.hh())
}
\keyword{color}
