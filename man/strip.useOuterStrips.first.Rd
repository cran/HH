\name{strip.useOuterStrips.first}
\alias{strip.useOuterStrips.first}
\alias{strip.useOuterStrips.last}
\alias{strip.left.useOuterStrips}
\alias{strip.top2}
\alias{strip.top1}
\alias{strip.left2}
\alias{strip.left1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions based on strip.default for use with the useOuterScales function.
}
\description{
Functions based on \code{strip.default} for use with the
\code{\link{useOuterScales}} function.  See \code{useOuterScales} for more
information.
}
\usage{
strip.useOuterStrips.first(which.given, which.panel, var.name, ...)
strip.useOuterStrips.last(which.given, which.panel, var.name, ...)
strip.left.useOuterStrips(which.given, which.panel, var.name, ...)
strip.top2(which.given, which.panel, var.name, ...)
strip.top1(which.given, which.panel, var.name, ...)
strip.left2(which.given, which.panel, var.name, ...)
strip.left1(which.given, which.panel, var.name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which.given, which.panel, var.name, \dots}{
See \code{\link[lattice]{strip.default}}.
}
}
\details{
The appropriate function is chosen by specifying arguments to
\code{\link{useOuterScales}}.

\code{strip.useOuterStrips.first} places strip labels at the top of the
first row of lattice panels.  Used when \code{as.table==TRUE}.

\code{strip.useOuterStrips.last} places strip labels at the top of the
first row of lattice panels.  Used when \code{as.table==FALSE}.

\code{strip.left.useOuterStrips} places strip labels at the left of the
first column of lattice panels.

\code{strip.top2} places row strip labels at the top of each panel.

\code{strip.top1} places column strip labels at the top of each panel.

\code{strip.left2} places row strip labels at the left of each panel.

\code{strip.left1} places column strip labels at the left of each panel.
}
\value{
See \code{\link[lattice]{strip.default}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }


\seealso{
\code{\link{useOuterScales}}
}
\examples{
## See examples in ?useOuterScales
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
