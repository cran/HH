\name{arima.diag.hh}
\alias{arima.diag.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Repair design error in S-Plus arima.diag}
\description{
  Repair design error in S-Plus \code{arima.diag}.
}
\usage{
arima.diag.hh(z, acf.resid = TRUE,
              lag.max = round(max(gof.lag + n.parms + 1, 10 * log10(n))),
              gof.lag = 15, resid = FALSE,
              std.resid = TRUE, plot = TRUE, type = "h", ...,
              x=eval(parse(text = series.name), local = sys.parent()))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z,acf.resid,lag.max,gof.lag,resid,std.resid,plot,type,\dots}{
  See \code{\link[splus]{arima.diag}}.}
\item{x}{The time series.  This must be specified when arima.diag is
called from inside another function.}
}
\details{
  Repairs design flaw in S-Plus \code{arima.diag}.  The location of the time
  series is hardwired one level up, so it can't be found when
  \code{arima.diag}
  is not one level down from the top.

  This function is a no-op in R.
}
\value{
  See \code{\link[splus]{arima.diag}}.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{tsdiagplot}}, \code{\link[splus]{arima.diag}}}
\keyword{ ts }