\name{tsdiagplot}
\alias{tsdiagplot}
\alias{acfplot}
\alias{aicsigplot}
\alias{residplot}
\alias{gofplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Times series diagnostic plots for a structured set of
  ARIMA models.}
\description{
Times series diagnostic plots for a structured set of ARIMA models.
}
\usage{
tsdiagplot(x,
           p.max=2, q.max=p.max,
           model=c(p.max, 0, q.max), ## S-Plus
           order=c(p.max, 0, q.max), ## R
           lag.max=36, gof.lag=lag.max,
           armas=if.R(
             s=arma.loop(x, model=model,
               series=deparse(substitute(x)), ...),
             r=arma.loop(x, order=order,
               series=deparse(substitute(x)), ...)),
           diags=diag.arma.loop(armas, x,
                                lag.max=lag.max,
                                gof.lag=gof.lag),
           ts.diag=rearrange.diag.arma.loop(diags),
           lag.units=ts.diag$tspar["frequency"],
           lag.lim=range(pretty(ts.diag$acf$lag))*lag.units,
           lag.x.at=pretty(ts.diag$acf$lag)*lag.units,
           lag.x.labels={tmp <- lag.x.at
                      tmp[as.integer(tmp)!=tmp] <- ""
                      tmp},
           lag.0=TRUE,
           main, lwd=0,
           ...)

acfplot(rdal, type="acf",
        main=paste("ACF of std.resid:", rdal$series,
                   "   model:",         rdal$model),
        lag.units=rdal$tspar["frequency"],
        lag.lim=range(pretty(rdal[[type]]$lag)*lag.units),
        lag.x.at=pretty(rdal[[type]]$lag)*lag.units,
        lag.x.labels={tmp <- lag.x.at
                      tmp[as.integer(tmp)!=tmp] <- ""
                      tmp},
        lag.0=TRUE,
        xlim=xlim.function(lag.lim/lag.units),
        ...)

aicsigplot(z, z.name=deparse(substitute(z)), series.name="ts",
           model=NULL,
           xlab="", ylab=z.name,
           main=paste(z.name,  series.name, model),
           layout=c(1,2), between=list(x=1,y=1), ...)

residplot(rdal,
          main=paste("std.resid:", rdal$series,
                     "   model:",  rdal$model),
          ...)

gofplot(rdal,
        main=paste("P-value for gof:", rdal$series,
                   "   model:",         rdal$model),
       lag.units=rdal$tspar["frequency"],
        lag.lim=range(pretty(rdal$gof$lag)*lag.units),
        lag.x.at=pretty(rdal$gof$lag)*lag.units,
        lag.x.labels={tmp <- lag.x.at
                      tmp[as.integer(tmp)!=tmp] <- ""
                      tmp},
        xlim=xlim.function(lag.lim/lag.units),
        pch=16, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Time series vector.}
  \item{p.max, q.max}{Maximum number of AR and MA arguments to use in
    the series of ARIMA models.}
  \item{model}{A valid S-Plus \code{model} for
%% R
#ifndef S-Plus
    \code{arima.mle}.
#endif
#ifdef S-Plus
    \code{\link[splus]{arima.mle}}.
#endif
  }
  \item{order}{A valid R \code{order} for
%% R
#ifndef S-Plus
    \code{\link[stats]{arima}}.
#endif
#ifdef S-Plus
    \code{arima}.
#endif
    The additional argument \code{seasonal} may also be used.}
  \item{lag.max}{Maximum lag for the acf and pacf plots.}
  \item{gof.lag}{Maximum lag for the gof plots.}
  
  \item{armas}{An \code{arma.loop} object.}
  \item{diags}{An \code{diag.arma.loop} object.}
  \item{ts.diag, rdal}{A list constructed as a rearranged \code{diag.arma.loop} object.}
  \item{lag.units}{Units for time series, defaults to \code{frequency(x)}}
  \item{lag.lim}{scaling for \code{xlim} in acf and pacf plots.}
  \item{lag.x.at, lag.x.labels}{Location of ticks and labels for the acf and pacf plots.}
  \item{lag.0}{Logical.  If \code{TRUE}, then plot the correlation (identically 1)
     at lag=0.
  If \code{FALSE}, do not plot the correlation at lag=0.}
  \item{type}{\code{"acf"} or \code{"pacf"}}
  \item{z}{A matrix constructed as the \code{aic} or \code{sigma2} component of the
  sumamry of a  \code{arma.loop} object.}
  \item{z.name}{\code{"aic"} or \code{"sigma2"}}
  \item{series.name}{Character string describing the time series.}
  \item{xlab, ylab, layout, between, pch, xlim, main, lwd}{Standard
  trellis arguments.}
  \item{\dots}{Additional arguments.  \code{tsdiagplot} sends them to
  \code{arima} or \code{arima.mle}.  \code{acfplot},
  \code{aicsigplot} \code{residplot}, and \code{gofplot} send them to \code{xyplot}.}
}
\value{
  \code{tsdiagplot} returns a \code{"tsdiagplot"} object which is
  a list of \code{"trellis"} objects.  It is printed with its own
  print method.

  The other functions return \code{"trellis"} objects.
}
\references{
  "Displays for Direct Comparison of ARIMA Models"
  The American Statistician, May 2002, Vol. 56, No. 2, pp. 131-138.
  Richard M. Heiberger, Temple University, and
  Paulo Teles, Faculdade de Economia do Porto.
  
  Richard M. Heiberger and Burt Holland (2004), Statistical Analysis and Data
  Display, Springer, ISBN 0-387-40270-5

}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{tsacfplots}}, \code{\link{arma.loop}}}
\examples{
data(tser.mystery.X)
X <- tser.mystery.X

X.dataplot <- tsacfplots(X, lwd=1, pch.seq=16, cex=.7)
X.dataplot

X.loop <- if.R(
               s=
               arma.loop(X, model=list(order=c(2,0,2)))
               ,r=
               arma.loop(X, order=c(2,0,2))
               )
X.dal <- diag.arma.loop(X.loop, x=X)
X.diag <- rearrange.diag.arma.loop(X.dal)
X.diagplot <- tsdiagplot(armas=X.loop, ts.diag=X.diag, lwd=1)
X.diagplot

X.loop
X.loop[["1","1"]]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
