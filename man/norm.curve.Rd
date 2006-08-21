\name{norm.curve}
\alias{norm.setup}
\alias{norm.curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot a normal curve with both x and z axes.}
\description{
  Plot a normal curve with both x (with \code{mean} and \code{se}
  as specified) and z (mean=0, se=1) axes.
  Shade a region for rejection region, acceptance region, confidence
  interval.
  The density axis is marked in units appropriate for the z axis.
  The existence of any of the arguments \code{se}, \code{sd}, \code{n}
  forces dual \code{x} and \code{z} scales.  When none of these
  arguments
  are used, the main title defaults to
  \code{"Standard Normal Density N(0,1)"} and only the \code{z} scale is
  printed.  A second density curve, appropriate for an alternative
  hypothesis
  is displayed when the argument \code{axis.name="z1"} is specified.    
}
\usage{
norm.setup(xlim.in=c(-2.5,2.5),
           ylim.in = c(0, 0.4)/se,
           mean=0,
           main.in=ifelse(
             !(missing(se) && missing(sd) && missing(n)),
             paste("normal density:  se =", round(se,3)),
             "Standard Normal Density N(0,1)"),
           se=sd/sqrt(n), sd=1, n=1, ...)

norm.curve(mean=0, se=sd/sqrt(n),
         critical.values=mean + se*c(-1.96, 1.96),
         z=do.call("seq",
                   as.list(c((par()$usr[1:2]-mean)/se, length=109))),
         shade, col=par("col"), axis.name="z", sd=1, n=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim.in, ylim.in}{\code{xlim, ylim}.
    Defaults to correct values for standard
           Normal(0,1).  User must set values for other mean and standard
           error.}
  \item{mean}{Mean of the normal distribution in xbar-scale,
    used in calls to \code{dnorm}.}
  \item{se}{standard error of the normal distribution in xbar-scale,
    used in calls to \code{dnorm}.}
  \item{sd, n}{standard deviation and sample size of the normal
    distribution in x-scale.  These may be used as an alternate way of
    specifying the standard error \code{se}.}
  \item{critical.values}{Critical values in xbar-scale.
    A scalar value implies a one-sided test.  A vector of two values
    implies a two-sided test.}
  \item{main.in}{Main title.}
  \item{z}{z-values (standardized to N(0,1)) used as base of plot.}
  \item{shade}{
    Valid values for shade are "right", "left", "inside", "outside".
    Default is "right" for one-sided critical.values and "outside"
    for two-sided critical values.}
  \item{col}{color of the shaded region.}
  \item{axis.name}{\code{"z"} for the standard normal scale centered on
    the null hypothesis value of the mean.
    \code{"z1"} for the standard normal scale centered on
    the alternate hypothesis value of the mean.}
  \item{\dots}{Other arguments which are ignored.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
old.par <- par(oma=c(4,0,2,5), mar=c(7,7,4,2)+.1)

norm.setup()
norm.curve()

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*(1.645))

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*(-1.645), shade="left")

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(mean=100, se=5, col=2)

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*c(-1.96, 1.96))

norm.setup(xlim=c(-3, 6))
norm.curve(crit=1.645, mean=1.645+1.281552, col=3,
           shade="left", axis.name="z1")
norm.curve(crit=1.645, col=2)

norm.setup(xlim=c(-6, 12), se=2)
norm.curve(crit=2*1.645, se=2, mean=2*(1.645+1.281552),
           col=3, shade="left", axis.name="z1")
norm.curve(crit=2*1.645, se=2, mean=0,
           col=2, shade="right")


par(mfrow=c(2,1))
norm.setup()
norm.curve()
mtext("norm.setup(); norm.curve()", side=1,  line=5)
norm.setup(n=1)
norm.curve(n=1)
mtext("norm.setup(n=1); norm.curve(n=1)", side=1,  line=5)
par(mfrow=c(1,1))


par(mfrow=c(2,2))

## naively scaled,
## areas under the curve are numerically the same but visually different
norm.setup(n=1)
norm.curve(n=1)
norm.setup(n=2)
norm.curve(n=2)
norm.setup(n=4)
norm.curve(n=4)
norm.setup(n=10)
norm.curve(n=10)
mtext("areas under the curve are numerically the same but visually different",
      side=3, outer=TRUE)

## scaled so all areas under the curve are numerically and visually the same
norm.setup(n=1, ylim=c(0,1.3))
norm.curve(n=1)
norm.setup(n=2, ylim=c(0,1.3))
norm.curve(n=2)
norm.setup(n=4, ylim=c(0,1.3))
norm.curve(n=4)
norm.setup(n=10, ylim=c(0,1.3))
norm.curve(n=10)
mtext("all areas under the curve are numerically and visually the same",
      side=3, outer=TRUE)

par(mfrow=c(1,1))


par(old.par)
}
\keyword{ aplot }
\keyword{ hplot }
\keyword{distribution}
