\name{panel.acf}
\alias{panel.acf}
\alias{panel.std.resid}
\alias{panel.gof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel functions for tsdiagplot.}
\description{
  Panel functions for \code{tsdiagplot}.
}
\usage{
panel.acf(..., n.used)
panel.std.resid(...)
panel.gof(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{standard arguments to panel functions.}
  \item{n.used}{number of lags.  THis number is needed to calculate the
    confidence interval which is \code{2//sqrt(n.used)}.}
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{tsdiagplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
