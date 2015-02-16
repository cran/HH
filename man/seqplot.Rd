 \name{seqplot}
\alias{seqplot}
\alias{seqplot.default}
%%\alias{seqplot.rts}
\alias{seqplot.ts}
%% \alias{seqplot.its}
%% \alias{seqplot.cts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time series plot.}
\description{
  Time series plot.
}
\usage{
seqplot(xts, ...)

\method{seqplot}{default}(xts,
        pch.seq=letters,
        groups=as.numeric(cycle(xts)),
        a=NULL, b=NULL, h=NULL, v=NULL,
        ylab=deparse(substitute(xts)),
        xlab="Time",
        lwd=1, lty=c(1,3),
        type="b",
        col=trellis.par.get("superpose.symbol")$col,
        col.line="gray60",
        ...)

%\method{seqplot}{rts}(xts, pch.seq=letters, groups=as.numeric(cycle(xts)),
%        x.at=pretty(time(xts)[groups==min(groups)]),
%        x.labels,
%        ylab=deparse(substitute(xts)),
%        ...)

\method{seqplot}{ts}(xts, pch.seq=letters, groups=as.numeric(cycle(xts)),
        x.at=pretty(time(xts)[groups==min(groups)]),
        x.labels,
        ylab=deparse(substitute(xts)),
        ...)

% \method{seqplot}{its}(xts,
%         pch.seq=letters,
%         groups=stop("must define groups for its"),
%         x.at=as.numeric(time(xts))[groups==min(groups)],
%         x.labels,
%         ylab=deparse(substitute(xts)),
%             ...)


% \method{seqplot}{cts}(xts,
%         pch.seq=letters,
%         groups=stop("must define groups for its"),
%         x.at=as.numeric(time(xts))[groups==min(groups)],
%         x.labels,
%         ylab=deparse(substitute(xts)),
%         ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xts}{Time series}
  \item{pch.seq}{sequence of \code{pch} characters for use with the time
            series.  The characters repeat over the cycle of the series.}
  \item{groups}{Numeric vector used to choose the plotting characters
            over cycles.}
  \item{a, b, h, v}{Arguments to \code{panel.abline}.}
  \item{ylab, xlab, lwd, lty, type}{ standard trellis arguments.}
  \item{x.at, x.labels}{shortcut for \code{scales=list(x=list(at=x.at,
      labels=x.labels))}}
  \item{col}{Color of dots in sequence plot.  The default is to make the
  choose a number of colors to match the frequency of the time series \code{xts}.}
  \item{col.line}{Color of connecting lines.  The default is \code{"gray60"}.}
  \item{\dots}{Additional arguments to \code{xyplot}.}
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{tsacfplots}}}
\examples{
seqplot(co2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
