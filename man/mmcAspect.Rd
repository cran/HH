\name{mmcAspect}
\alias{mmcAspect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Control aspect ratio in MMC plots to maintain isomeans grid as a square.
}
\description{
Control aspect ratio in MMC plots to maintain isomeans grid as a square.
}
\usage{
mmcAspect(trellis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trellis}{
A \code{trellis} object.  If there is more than one panel, the first
panel will be used.
}
}
\value{
New numeric aspect ratio that will force the isomeans grid to be a
square rotated to have vertical and horizontal diagonals.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{mmcplot}}
}
\keyword{hplot}
