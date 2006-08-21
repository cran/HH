\name{export.eps}
\alias{export.eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exports a graph to an EPS file.}
\description{
  Exports a graph from the current device in R,
  or the \code{graphsheet} in S-Plus, to an \code{EPS} file.
}
\usage{
export.eps(FileName.in, Name.in="GSD2", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName.in}{ name of file to be created.}
  \item{Name.in}{Name of graphsheet in S-Plus, ignored in R.}
  \item{\dots}{other arguments in R, ignored in S-Plus.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[grDevices]{dev.copy2eps}} in R,
  \code{\link[sgui]{export.graph}} in S.}
\examples{
\dontrun{
trellis.device()
plot(1:10)
export.eps("abcd.eps")
}
}
\keyword{ device }
