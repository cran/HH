\name{summary.arma.loop}
\alias{summary.arma.loop}
\alias{summary.arma.loop.list}
\alias{print.arma.loop}
\alias{print.arma.loop.list}
\alias{print.tsacfplots}
\alias{[.arma.loop}
\alias{[.diag.arma.loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{summary and print and subscript methods for tsdiagplot and related objects.}
\description{
  summary and print and subscript methods for tsdiagplot and related objects.
}
\usage{
summary.arma.loop(object, ...)
summary.arma.loop.list(object, ...)
print.arma.loop(x, ...)
print.arma.loop.list(x, ...)
print.tsacfplots(x, ...)
`[.arma.loop`(x, ..., drop = TRUE)
`[.diag.arma.loop`(x, ..., drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object to be summarized or printed or subscripted.}
  \item{\dots}{additional arguments}
  \item{drop}{See \code{\link[base]{Extract}} in R, \code{\link[main]{Subscript}} in S-Plus.}
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{arma.loop}}, \code{\link{tsacfplots}}, \code{\link{tsdiagplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
