\name{matrix.trellis}
\alias{matrix.trellis}
\alias{as.matrix.trellis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a one-dimensional trellis object to a two-dimensional trellis object.
This permits combineLimits and useOuterStrips to work.
}
\description{
matrix.trellis
}
\usage{
matrix.trellis(x = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)

\method{as.matrix}{trellis}(x, ..., row = FALSE, yname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x
}
  \item{nrow, ncol, byrow, dimnames}{
See \code{\link[base]{matrix}}.
}
  \item{row}{
Logical.  The default is \code{FALSE} to match the behavior of the
generic \code{\link[base]{as.matrix}}.  I think \code{TRUE} usually
looks better.
}
  \item{yname}{
Character. Provides the name of the generated conditioning factor.
}
  \item{\dots}{
Other arguments are ignored.
}
}
\details{
  \code{matrix.trellis} lets the user specify \code{nrow} and \code{ncol}.
  \code{as.matrix.trellis} produces either be a single column (by
  default) or a single row.
}
\value{
\code{trellis} object with \code{length(dim(x)) == 2} and specified nrow and ncol.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tmp <- data.frame(a=1:3,
                  b=c(4,5,7),
                  c=5:7,
                  d=c(8, 9, 12),
                  e=9:11)
tmp
a1 <- xyplot(a + b ~ c + d + e, data=tmp, outer=TRUE,
             main="a1")
a1
dim(a1)
a2 <- xyplot(a + b ~ c + d + e, data=tmp, outer=TRUE,
             scales=list(relation="free"), main="a2")
a2
dim(a2)
try(combineLimits(a2))
combineLimits.trellisvector(a2)
combineLimits.trellisvector(update(a2, layout=c(3,2)))

a21 <- matrix.trellis(a2, ncol=3, nrow=2, byrow=TRUE)
a21 <- update(a21, main="a21")
a21
dim(a21)
a21$x.scales$at
combineLimits(a21)

a22 <- update(a21, main="a22")
a22$x.scales$at <- list(FALSE, FALSE, FALSE, NULL, NULL, NULL)
a22$y.scales$at <- list(FALSE, NULL, NULL, FALSE, NULL, NULL)
a22

a23 <- useOuterStrips(combineLimits(a21))
a23 <- update(a23, main="a23")
a23
a23$condlevels
a23$condlevels <- list(letters[3:5], letters[1:2])
a23

a24 <- resizePanels(update(a23, main="a24"), h=c(3,4), w=c(3,5,3))
a24

a25 <- update(a23, xlab=letters[3:5], ylab.right=letters[1:2],
              xlab.top="column variables",
              ylab="row variables",
              scales=list(x=list(alternating=1), y=list(alternating=2)),
              main="a25: what I want\nxyplot(a + b ~ c + d + e, data=tmp, outer=TRUE)\nto produce.")
a25

as.matrix(a1)
as.matrix(a1, yname="abcd")
as.matrix(a1, yname="abcd", row=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
