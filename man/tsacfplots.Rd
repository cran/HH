\name{tsacfplots}
\alias{tsacfplots}
\alias{acf.pacf.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coordinated time series and ACF and PCF plots.}
\description{
  Coordinated time series and ACF and PCF plots.
}
\usage{
tsacfplots(x,      
           ylab=deparse(substitute(x)),
           x.name=ylab[[1]],
           main=paste("Series:", x.name),
           lag.at=NULL,
           lag.max=NULL,
           lag.units=NULL,
           lag.0=TRUE,
           ...)

acf.pacf.plot(x,
	      ylab=NULL,
	      series=deparse(substitute(x)),
	      main=paste("ACF and PACF:", series),
	      lag.max,
	      lag.units=frequency(x),
	      lag.at=pretty(apacf$lag),
	      lag.labels=lag.at*lag.units,
              lag.0=TRUE,
	      ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series}
  \item{ylab, main}{standard trellis arguments.}
  \item{x.name, series}{Character string, name for the time series.}
  \item{lag.at}{Location of ticks for the acf and pacf plots.}
  \item{lag.labels}{Labels for ticks for the acf and pacf plots.}
  \item{lag.max}{Maximum lag used in the acf and pacf plots.}
  \item{lag.units}{Units for time series, defaults to \code{frequency(x)}}
  \item{lag.0}{Logical.  If \code{TRUE}, then plot the correlation (identically 1)
     at lag=0.
  If \code{FALSE}, do not plot the correlation at lag=0.}
  \item{\dots}{Additional arguments to \code{seqplot} for \code{tsacfplots}.
Additional arguments to \code{strip.default} for \code{acf.pacf.plot}.}
}
\details{
 The acf and pacf plots are scaled identically.
}
\value{
\code{"tsacfplots"} object containing two \code{"trellis"} objects.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{seqplot}}}
\examples{
tsacfplots(co2)
acf.pacf.plot(co2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
