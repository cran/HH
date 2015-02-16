\name{as.vector.trellis}
\alias{as.vector.trellis}
\alias{reorder.trellis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a two-dimensional trellis object into a one-dimensional trellis object.
Change the order of panels in a trellis object.
}
\description{
\code{as.vector.trellis} converts a two-dimensional trellis object into
a one-dimensional trellis object.
\code{reorder.trellis} changes the order of the \code{panel.args} component in a trellis object.
These are are mostly used as utilities by \code{\link{matrix.trellis}}.
}
\usage{
\method{as.vector}{trellis}(x, mode = "any")
\method{reorder}{trellis}(x, X, ...)
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
  \item{\dots}{
Other arguments are ignored.
}
  \item{X}{
Subscript vector specifying the new order of the panels.
}
}
\value{
  trellis object with \code{length(dim(x)) == 1}.
  \code{as.vector} retains the original order of the
  panels. \code{reorder} changes the order to the one specified by using
  the \code{X} argument as a subscript.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tmp <- data.frame(a=letters[c(1:3,1:3,1:3)],
                  b=1:9,
                  d=1:9,
                  e=LETTERS[c(4,4,4,5,5,5,6,6,6)])
tmp
a6 <- xyplot(b ~ d | a*e, data=tmp, pch=19)
a6
dim(a6)
a62 <- as.vector(a6)
a62
dim(a62)
a63 <- reorder(a6, c(1,4,7, 2,5,8, 3,6,9))
a63
dim(a63)
a64 <- matrix.trellis(a63, nrow=3, ncol=3, dimnames=dimnames(a6), byrow=TRUE)
a64
dim(a64)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
