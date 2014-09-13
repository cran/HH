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
                      position=list(
                        A321=c(0,     0.27, 1, 1   ),  ## these values work
                        A4  =c(0.005, 0,    1, 0.30))) ## for pdf(height=9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{latticeresids} object.
}
  \item{\dots}{
Other arguments for \code{print}.
}
  \item{position}{
    This is the position argument for \code{\link[lattice]{print.trellis}}.
    The default values work for the pdf device with height=9.
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
