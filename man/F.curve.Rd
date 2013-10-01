
\name{F.curve}
\alias{chisq.curve}
\alias{chisq.observed}
\alias{chisq.setup}
\alias{F.curve}
\alias{F.observed}
\alias{F.setup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot a chisquare or a F-curve.}
\description{
  Plot a chisquare or a F-curve.  Shade a region for
  rejection region or do-not-reject region.  \code{F.observed} and
  \code{chisq.observed} plots a vertical line with arrowhead markers at
  the location of the observed xbar and outlines the area corresponding
  to the \eqn{p}-value.  }
\usage{
F.setup(df1=1,
        df2=Inf,
        ncp=0,
        log.p=FALSE,
        xlim.in=c(0, 5),
        ylim.in=range(c(0, 1.1*df.intermediate(x=seq(.5,1.5,.01),
                       df1=df1, df2=df2, ncp=ncp, log=log.p))),
        main.in=main.calc,
        ...)

F.curve(df1=1,
        df2=Inf,
        ncp=0,
        log.p=FALSE,
        alpha=.05,
        critical.values=f.alpha,
        f=seq(0, par()$usr[2], length=109),
        shade="right", col=par("col"),
        axis.name="f",
        ...)

F.observed(f.obs, col="green",
           df1=1,
           df2=Inf,
           ncp=0,
           log.p=FALSE,
           axis.name="f",
           shade="right",
           shaded.area=0,
           display.obs=TRUE)

chisq.setup(df=1,
           ncp=0,
           log.p=FALSE,
           xlim.in=c(0, qchisq.intermediate(p=1-.01, df=df, ncp=ncp, log.p=log.p)),
           ylim.in=range(c(0, 1.1*dchisq.intermediate(x=seq(max(0.5,df-2),df+2,.01),
                         df=df, ncp=ncp, log=log.p))),
           main.in=main.calc,
           ...)

chisq.curve(df=1,
            ncp=0,
            log.p=FALSE,
            alpha=.05,
            critical.values=chisq.alpha,
            chisq=seq(0, par()$usr[2], length=109),
            shade="right", col=par("col"),
            axis.name="chisq",
            ...)

chisq.observed(chisq.obs, col="green",
               df=1,
               ncp=0,
               log.p=FALSE,
               axis.name="chisq",
               shade="right",
               shaded.area=0,
               display.obs=TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim.in, ylim.in}{Initial settings for \code{xlim, ylim}.
    The defaults are calculated for the degrees of freedom.}
  \item{df, df1, df2, ncp, log.p}{Degrees of freedom,
    non-centrality parameter, probabilities are given as log(p).
    See \code{\link{pchisq}} and \code{\link{pf}}.}
  \item{alpha}{Probability of a Type I error.  \code{alpha} is a vector
of
one or two values.  If one value, it is the right alpha.  If two values,
they are the \code{c(left.alpha, right.alpha)}.}
  \item{critical.values}{Critical values.  Initial values correspond
    to the specified \code{alpha} levels.
    A scalar value implies a one-sided test on the right side.
    A vector of two values
    implies a two-sided test.}
  \item{main.in}{Main title.}
  \item{shade}{
    Valid values for shade are "right", "left", "inside", "outside", "none".
    Default is "right" for one-sided critical.values and "outside"
    for two-sided critical values.}
  \item{col}{color of the shaded region and the area of the shaded region.}
  \item{shaded.area}{Numerical value of the area.  This value may
  be cumulated over two calls to the function (one call for left, one
  call for right).
  The \code{shaded.area} is the return value of the function.
  The calling program is responsible for the
  cumulation.}
  \item{display.obs}{Logical.  If \code{TRUE}, print the numerical value
    of the observed value, plot a vertical \code{abline} at the value,
    and use it for showing the \eqn{p}-value.
    If \code{FALSE}, don't print or plot the observed value; just use it
  for showing the \eqn{p}-value.}
  \item{f,chisq}{Values used to draw curve.  Replace them if more
    resolution is needed.}
  \item{f.obs, chisq.obs}{Observed values of statistic.  \eqn{p}-values are
    calculated for these values.}
  \item{axis.name}{Axis name.}
  \item{\dots}{Other arguments which are ignored.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
old.omd <- par(omd=c(.05,.88, .05,1))
chisq.setup(df=12)
chisq.curve(df=12, col='blue')
chisq.observed(22, df=12)
par(old.omd)

old.omd <- par(omd=c(.05,.88, .05,1))
chisq.setup(df=12)
chisq.curve(df=12, col='blue', alpha=c(.05, .05))
par(old.omd)

old.omd <- par(omd=c(.05,.88, .05,1))
F.setup(df1=5, df2=30)
F.curve(df1=5, df2=30, col='blue')
F.observed(3, df1=5, df2=30)
par(old.omd)

old.omd <- par(omd=c(.05,.88, .05,1))
F.setup(df1=5, df2=30)
F.curve(df1=5, df2=30, col='blue', alpha=c(.05, .05))
par(old.omd)

}
\keyword{ aplot }
\keyword{ hplot }
\keyword{distribution}
