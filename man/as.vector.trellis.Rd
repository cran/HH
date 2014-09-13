\name{as.vector.trellis}
\alias{as.vector.trellis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a two-dimensional trellis object into a one-dimensional trellis object.
}
\description{
Convert a two-dimensional trellis object into a one-dimensional trellis object.
It is mostly used as a utility by \code{\link{matrix.trellis}}.
}
\usage{
\method{as.vector}{trellis}(x, mode = "any")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{trellis} object.
}
  \item{mode}{
We are hijacking the \code{mode} argument.  It is used here for the
names of the panels.
}
}
\value{
trellis object with length(dim(x)) == 1
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tmp <- data.frame(a=factor(1:3),
                  b=c(4,5,7),
                  c=factor(5:7),
                  d=c(8, 9, 12),
                  e=factor(9:11))
tmp
a6 <- xyplot(b ~ d | a*e, data=tmp)
a6
dim(a6)
a62 <- as.vector(a6)
a62
dim(a62)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
