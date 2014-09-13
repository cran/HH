\name{panel.confintMMC}
\alias{panel.confintMMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence interval panel for MMC tiebreaker plots, or confidence
interval plot.
}
\description{
Confidence interval panel for MMC tiebreaker plots, or confidence
interval plot.
}
\usage{
panel.confintMMC(x, y, subscripts, ..., col, lty, lower, upper, contrast.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
means
}
  \item{y}{
    When called from \code{mmcisomeans}, the heights associated with the contrasts.
    When called from \code{mmcmatch}, integers from one to the number of means.
}
  \item{subscripts}{
Index into the \code{contrast.names}.
}
  \item{\dots}{
Additional arguments are ignored.
}
  \item{col, lty}{
Standard \pkg{lattice} arguments.
}
  \item{lower}{
Vector of lower bounds for the intervals.
}
  \item{upper}{
Vector of upper bounds for the intervals.
}
  \item{contrast.name}{
Names of the contrasts.
}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
See \code{\link{mmc}} for the references and examples.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
