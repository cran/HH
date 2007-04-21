\name{legendGrob2wt}
\alias{legendGrob2wt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{place separate keys to the left of each row of a trellis}
\description{
Each key is created and then inserted into a single grob.
}
\usage{
legendGrob2wt(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{key1, key2, etc.  Each key will normally be the result of
    a \code{draw.key} with \code{draw=FALSE}.}
}
\value{
  A Grid frame object (that inherits from 'grob').
}
\author{Richard M. Heiberger, with asssistance from Deepayan Sarkar.}
\seealso{\code{\link{interaction2wt}}}
\keyword{dplot}
