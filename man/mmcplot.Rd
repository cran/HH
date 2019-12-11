\name{mmcplot}
\alias{mmcplot}
\alias{mmcplot.mmc}
\alias{mmcplot.glht}
\alias{mmcplot.mmc.multicomp}
\alias{mmcplot.multicomp}
\alias{mmcplot.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MMC (Mean-mean Multiple Comparisons) plots in lattice.
}
\description{
MMC (Mean--mean Multiple Comparisons) plots in lattice
}
\usage{
mmcplot(mmc, ...)
\method{mmcplot}{mmc}(mmc, col=c("black","red"), lwd=c(1,1), lty=c(2,1), ...,
                  style=c("isomeans", "confint", "both"),
                  type=c("mca", "lmat", "linfct", "none"))
\method{mmcplot}{glht}(mmc, col=c("black","red"), lwd=c(1,1), lty=c(2,1),
                  focus=mmc$focus, ...)
\method{mmcplot}{mmc.multicomp}(mmc, col=c("black","red"), lwd=c(1,1), lty=c(2,1), ...)
\method{mmcplot}{multicomp}(mmc, col=c("black","red"), lwd=c(1,1), lty=c(2,1), ...)
\method{mmcplot}{default}(mmc, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mmc}{
\code{mmc} object or other object as indicated by method.
}
  \item{col, lwd, lty}{Standard
        \code{\link[lattice]{xyplot}} arguments applied to the line segments
        representing the contrasts.
}
\item{focus}{Name of the factor for which the \code{glht} object was constructed.}
\item{\dots}{
Other arguments to be passed on to the functions called by the methods.
}
  \item{style}{
Style of graph: The default \code{isomeans} is the standard MMC plot with the
isomeans grid.  \code{confint} is a confidence interval plot, similar to
the plot produced by \code{multcomp:::plot.glht}.
\code{both} prints both the \code{isomeans} and
the \code{confint} plot as two panels of a \code{trellis} structure.
When the underlying sets of means are close to each other, there will
of necessity be overprinting in the \code{isomeans} panel and the
\code{confint} panel will be needed as a tiebreaker.
By default the \code{xlim} for the \code{confint} style will match the
\code{xlim} of the corresponding \code{isomeans} plot.
}
\item{type}{
  \code{mca} for the default paired-comparisons plot.  \code{lmat} or
  \code{linfct}
  for a user-specified set of contrasts.  \code{none} for confidence
  intervals on the set of group means (that is, no comparisons).
  }
}

\value{
A \code{trellis} object containing the graphs.
}
\references{
See \code{\link{mmc}} for the references.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{mmc}} for the discussion of the MMC and for many examples.
The functions \code{\link{mmcisomeans}}, \code{\link{mmcmatch}},
\code{\link{mmcboth}} are the internal functions that do the
actual work of plotting.
}
\examples{
data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
catalystm.mmc <-
   mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
mmcplot(catalystm.mmc)
mmcplot(catalystm.mmc, style="both", MMCname="catalyst")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
