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
\method{summary}{arma.loop}(object, ...)
\method{summary}{arma.loop.list}(object, ...)
\method{print}{arma.loop}(x, ...)
\method{print}{arma.loop.list}(x, ...)
\method{print}{tsacfplots}(x, ...)
\method{[}{arma.loop}(x, ..., drop = TRUE)
\method{[}{diag.arma.loop}(x, ..., drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object to be summarized or printed or subscripted.}
  \item{\dots}{additional arguments}
  \item{drop}{See
%% R
#ifndef S-Plus
\code{\link[base]{Extract}}.
#endif
#ifdef S-Plus  
\code{\link[main]{Subscript}}.
#endif
  }
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{arma.loop}}, \code{\link{tsacfplots}}, \code{\link{tsdiagplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
