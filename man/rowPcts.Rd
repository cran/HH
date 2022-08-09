\name{rowPcts}
\alias{rowPcts}
\alias{colPcts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Row and columns percents
}
\description{
Row and columns percents.
}
\usage{
rowPcts(x, ...)
colPcts(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numerical matrix
}
  \item{\dots}{
Additional arguments for \code{\link{rowSums}}
}
}
\value{
Calculate percents by row or column.  The \code{rowSums} or
\code{colSums} are stored in the \code{Sums} attribute of the result.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rowSums}}
}
\examples{
tmp <- matrix(1:12, 3, 4,
              dimnames=list(c("A","B","C"),
                            c(letters[4:7])))
tmp
rowPcts(tmp)
colPcts(tmp)
}
