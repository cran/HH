\name{norm.curve}
\alias{norm.setup}
\alias{norm.curve}
\alias{norm.observed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot a normal or a t-curve with both x and z axes.}
\description{
  Plot a normal curve or a t-curve with both x (with \code{mean} and \code{se}
  as specified) and z or t (mean=0, se=1) axes.
  Shade a region for rejection region, acceptance region, confidence
  interval.
  The density axis is marked in units appropriate for the z or t axis.
  The existence of any of the arguments \code{se}, \code{sd}, \code{n}
  forces dual \code{x} and (\code{z} or \code{t}) scales.  When none of these
  arguments
  are used, the main title defaults to
  \code{"Standard Normal Density N(0,1)"} and only the \code{z} scale is
  printed.  A second density curve, appropriate for an alternative
  hypothesis
  is displayed when the argument \code{axis.name="z1"} is specified.
The shaded area is printed on the plot.
  
  When the optional argument \code{df.t} is specified, then a
  t-distribution with \code{df.t} degrees of freedom is plotted.

  \code{norm.observed} plots a vertical line with arrowhead markers at
  the location of the observed xbar.
}
\usage{
norm.setup(xlim.in=c(-2.5,2.5),
           ylim.in = c(0, 0.4)/se,
           mean=0,
           main.in=main.calc,
           se=sd/sqrt(n), sd=1, n=1,
           df.t=NULL,
            ...)

norm.curve(mean=0, se=sd/sqrt(n),
         critical.values=mean + se*c(-1, 1)*z.975,
         z=do.call("seq",
                   as.list(c((par()$usr[1:2]-mean)/se, length=109))),
         shade, col=par("col"),
         axis.name=ifelse(is.null(df.t) || df.t == Inf, "z", "t"),
         second.axis.label.line=3,
         sd=1, n=1,
         df.t=NULL,
         ...)

norm.observed(xbar, t.xbar, col="blue")
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
  \item{df.t}{Degrees of freedom for the t distribution.  When
         \code{df.t} is \code{NULL}, the normal distribution is used.}
  \item{critical.values}{Critical values in xbar-scale.
    A scalar value implies a one-sided test.  A vector of two values
    implies a two-sided test.}
  \item{main.in}{Main title.  Default value is:
    \code{
      if (is.null(df.t))  ## normal
        ifelse(!(missing(se) && missing(sd) && missing(n)),
               paste("normal density:  se =", round(se,3)),
               "Standard Normal Density N(0,1)")
      else { ## t distribution
        if (length(df.t) != 1) stop("df.t must have length 1")
        ifelse(!(missing(se) && missing(sd) && missing(n)),
               paste("t density:  se = ", round(se,3), ", df = ", df.t, sep=""),
               paste("t density, df =", df.t))
	     }
	   }
	 }
  \item{z}{z-values (standardized to N(0,1)) used as base of plot.}
  \item{shade}{
    Valid values for shade are "right", "left", "inside", "outside", "none".
    Default is "right" for one-sided critical.values and "outside"
    for two-sided critical values.}
  \item{col}{color of the shaded region and the area of the shaded region.}
  \item{axis.name}{defaults to \code{"z"}
    for the standard normal scale centered on
    the null hypothesis value of the mean or to \code{"t"} for
    the t distribution with \code{df.t} degrees of freedom.
    For alternative hypotheses, the user must specify either
    \code{"z1"} or \code{"t1"} for the standard normal scale,
    or t distibution with \code{df.t} degrees of freedom, centered on
    the alternate hypothesis value of the mean.}
  \item{second.axis.label.line}{Defaults to \code{3}.
    Normally not needed.  When two curves are drawn, one normal and one t,
    then the second curve needs a different label for the y-axis.
    Set this value to 4 to avoid overprinting.}
  \item{xbar}{xbar-value of the observed data.}
  \item{t.xbar}{t-value of the observed data under the null hypothesis.}
  \item{\dots}{Other arguments which are ignored.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
old.par <- par(oma=c(4,0,2,5), mar=c(7,7,4,2)+.1)

norm.setup()
norm.curve()

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*(1.645))
norm.observed(112, (112-100)/5)

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*(-1.645), shade="left")

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(mean=100, se=5, col='red')

norm.setup(xlim=c(75,125), mean=100, se=5)
norm.curve(100, 5, 100+5*c(-1.96, 1.96))

norm.setup(xlim=c(-3, 6))
norm.curve(crit=1.645, mean=1.645+1.281552, col='green',
           shade="left", axis.name="z1")
norm.curve(crit=1.645, col='red')

norm.setup(xlim=c(-6, 12), se=2)
norm.curve(crit=2*1.645, se=2, mean=2*(1.645+1.281552),
           col='green', shade="left", axis.name="z1")
norm.curve(crit=2*1.645, se=2, mean=0,
           col='red', shade="right")


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
norm.observed(1.2, 1.2/(1/sqrt(1)))
norm.setup(n=2)
norm.curve(n=2)
norm.observed(1.2, 1.2/(1/sqrt(2)))
norm.setup(n=4)
norm.curve(n=4)
norm.observed(1.2, 1.2/(1/sqrt(4)))
norm.setup(n=10)
norm.curve(n=10)
norm.observed(1.2, 1.2/(1/sqrt(10)))
mtext("areas under the curve are numerically the same but visually different",
      side=3, outer=TRUE)

## scaled so all areas under the curve are numerically and visually the same
norm.setup(n=1, ylim=c(0,1.3))
norm.curve(n=1)
norm.observed(1.2, 1.2/(1/sqrt(1)))
norm.setup(n=2, ylim=c(0,1.3))
norm.curve(n=2)
norm.observed(1.2, 1.2/(1/sqrt(2)))
norm.setup(n=4, ylim=c(0,1.3))
norm.curve(n=4)
norm.observed(1.2, 1.2/(1/sqrt(4)))
norm.setup(n=10, ylim=c(0,1.3))
norm.curve(n=10)
norm.observed(1.2, 1.2/(1/sqrt(10)))
mtext("all areas under the curve are numerically and visually the same",
      side=3, outer=TRUE)

par(mfrow=c(1,1))


## t distribution
mu.H0 <- 16
se.val <- .4
df.val <- 10
crit.val <- mu.H0 - qt(.95, df.val) * se.val
mu.alt <- 15
obs.mean <- 14.8

alt.t <- (mu.alt - crit.val) / se.val
norm.setup(xlim=c(12, 19), se=se.val, df.t=df.val)
norm.curve(crit=crit.val, se=se.val, df.t=df.val, mean=mu.alt,
           col='green', shade="left", axis.name="t1")
norm.curve(crit=crit.val, se=se.val, df.t=df.val, mean=mu.H0,
           col='gray', shade="right")
norm.observed(obs.mean, (obs.mean-mu.H0)/se.val)

## normal
norm.setup(xlim=c(12, 19), se=se.val)
norm.curve(crit=crit.val, se=se.val, mean=mu.alt,
           col='green', shade="left", axis.name="z1")
norm.curve(crit=crit.val, se=se.val, mean=mu.H0,
           col='gray', shade="right")
norm.observed(obs.mean, (obs.mean-mu.H0)/se.val)



## normal and t
norm.setup(xlim=c(12, 19), se=se.val, main="t(6) and normal")
norm.curve(crit=15.5, se=se.val, mean=16.3,
           col='gray', shade="right")
norm.curve(crit=15.5, se.val, df.t=6, mean=14.7,
           col='green', shade="left", axis.name="t1", second.axis.label.line=4)
norm.curve(crit=15.5, se=se.val, mean=16.3,
           col='gray', shade="none")

norm.setup(xlim=c(12, 19), se=se.val, main="t(6) and normal")
norm.curve(crit=15.5, se=se.val, mean=15.5,
           col='gray', shade="right")
norm.curve(crit=15.5, se=se.val, df.t=6, mean=15.5,
           col='green', shade="left", axis.name="t1", second.axis.label.line=4)
norm.curve(crit=15.5, se=se.val, mean=15.5,
           col='gray', shade="none")



par(old.par)
}
\keyword{ aplot }
\keyword{ hplot }
\keyword{distribution}
