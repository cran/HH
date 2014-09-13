\name{combineLimits.trellisvector}
\alias{combineLimits.trellisvector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine limits on a one-dimensional trellis object.
}
\description{
Combine limits on a one-dimensional trellis object.
}
\usage{
combineLimits.trellisvector(x, margin.x = 1:2, margin.y = 1:2,
                            layout = x$layout,
                            ncol=x$layout[1], nrow=x$layout[2],
                            condlevels = x$condlevels[[1]], ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{trellis} object.
}
  \item{margin.x, margin.y}{
See \code{\link[latticeExtra]{combineLimits}}.
}
  \item{layout}{
See \code{\link[lattice]{xyplot}}.
}
  \item{condlevels}{
    Character.  Names of each panel of the result.  Defaults to the names
    of the panels of the argument.
}
  \item{\dots}{
Other arguments are ignored.
}
  \item{nrow, ncol}{
    See \code{\link{matrix.trellis}}.  These arguments default to the levels
    of \code{x$layout} if it is non-null.
    Otherwise \code{nrow==1} and ncol==\code{dim(x)}.
}
}
\details{
The one-dimensional object is converted to a two-dimensional object
which
is forwarded to the standard \code{combineLimits} function.  The result
is converted back to a one-dimensional object.
}
\value{
One-dimensional trellis object with combined xlim and ylim values across
all panels.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[latticeExtra]{combineLimits}}
}
\examples{
tmp <- data.frame(a=1:3,
                  b=c(4,5,7),
                  c=5:7,
                  d=c(8, 9, 12),
                  e=9:11)
tmp

a2 <- xyplot(a + b ~ c + d + e, data=tmp, outer=TRUE,
             scales=list(relation="free"), main="a2")
a2
dim(a2)
combineLimits.trellisvector(a2)
a21 <- combineLimits.trellisvector(update(a2, layout=c(3,2)))
a21
dim(a21)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
