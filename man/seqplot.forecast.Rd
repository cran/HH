\name{seqplot.forecast}
%% \alias{seqplot.forecast} %% old name
\alias{seqplotForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{seqplot with confidence bands for the forecast region.}
\description{
  \code{seqplot} with confidence bands for the forecast region.
}
\usage{
seqplotForecast(xts, forecast, multiplier = 1.96,
                series = deparse(substitute(observed)), ylim,
                CI.percent=round((1-2*(1-pnorm(multiplier)))*100,2),
                main = paste(
                series, " with forecast + ",
                CI.percent, "\% CI", sep=""),
                xlab=NULL, ylab=NULL,
                ...) ## x.at, xlim
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xts}{This is the observed series}
  \item{forecast}{forecast values based on the model}
  \item{multiplier}{Half-width of confidence interval in standard normal
  units.  Defaults to 1.96.}
  \item{CI.percent}{Width of confidence band.  Defaults to the standard
    normal, two-sided value associated with the multiplier (95 percent
    for the default \code{multiplier=1.96}).}
  \item{series}{Name of time series will be used to construct the main
    title for the plot.}
  \item{ylim, xlab, ylab, main}{standard trellis parameters}
  \item{\dots}{additional arguments to \code{xyplot}.}
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{seqplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
