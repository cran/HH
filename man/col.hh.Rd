\name{col.hh}
\alias{col.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initializing Trellis Displays}
\description{
     Initialization of an R display device with the graphical
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
\dontrun{
if.R(r={
trellis.device(theme="col.hh") ## Open a device with the theme

trellis.device(theme=col.hh()) ## Open a device with the theme

trellis.par.set(theme=col.hh())## Add theme to already open device
},s={})
}
}
\keyword{color}
