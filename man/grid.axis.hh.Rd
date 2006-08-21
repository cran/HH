\name{grid.yaxis.hh}
\alias{grid.yaxis.hh}
\alias{make.yaxis.hh.labels}
\alias{grid.xaxis.hh}
\alias{make.xaxis.hh.labels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{make x- and y-axis labels}
\description{
  uses modified older version of grid functions.
  Includes optional specification of the axis labels.
}
\usage{
grid.yaxis.hh(at = NULL, label = TRUE, main = TRUE, gp = gpar(),
              draw = TRUE, vp = NULL, labels)

make.yaxis.hh.labels(at, main, labels = at)

grid.xaxis.hh(at = NULL, label = TRUE, main = TRUE, gp = gpar(),
              draw = TRUE, vp = NULL, labels)

make.xaxis.hh.labels(at, main, labels = at)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{at, label, main, gp, draw, vp}{See \code{link[grid]{grid.xaxis}}.}
  \item{labels}{label values if you don't want the defaults }
}
\value{
See \code{link[grid]{grid.xaxis}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{link[grid]{grid.xaxis}}}
\keyword{dplot}
