\name{panel.isomeans}
\alias{panel.isomeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isomeans grid for MMC plots.
}
\description{
isomeans grid for MMC plots.
}
\usage{
panel.isomeans(ybar,
               lty.iso=7,
               col.iso='darkgray',
               lwd.iso=1,
               lty.contr0=2,
               col.contr0='darkgray',
               lwd.contr0=1,
               ...,
               col, lty ## capture potentially ambiguous name
               )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ybar}{
Vector of means.
}
\item{lty.iso, col.iso, lwd.iso}{color, line type,
  line width for the isomeans grid.}

\item{lty.contr0, col.contr0, lwd.contr0}{color, line type,
  line width for the vertical contrast=0 line.}
\item{\dots}{ignore any additional arguments}
\item{col, lty}{ignore these arguments.  They are captured here to avoid
ambiguity with \code{col.iso} and \code{lty.iso}.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
See \code{\link{mmc}} for the references and examples.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
