\name{regr1.plot}
\alias{regr1.plot}
\title{plot x and y,
with optional straight line fit and display of squared residuals}
\description{
Plot \code{x} and \code{y},
with optional fitted line and display of squared residuals.
By default the least squares line is calculated and used.
Any other straight line
can be specified by placing its coefficients in \code{coef.model}.
Any other fitted model can be calculated by specifying the \code{model}
argument.
Any other function of one variable can be specified in the
\code{alt.function} argument.  At most one of the arguments
\code{model}, \code{coef.model}, \code{alt.function} can be specified.}
\usage{
regr1.plot(x, y,
           model=lm(y~x),
           coef.model,
           alt.function,
           main="put a useful title here",
           xlab=deparse(substitute(x)),
           ylab=deparse(substitute(y)),
           jitter.x=FALSE,
           resid.plot=FALSE,
           points.yhat=TRUE,
           pch=16,
           ..., length.x.set=51,
           x.name,
           pch.yhat=16,
           cex.yhat=par()$cex*.7,
           err=-1)
}
\arguments{
  \item{x}{x variable}
  \item{y}{y variable}
  \item{model}{Defaults to the simple linear model \code{lm(y ~ x)}.
    Any model object with one \code{x}
    variable, such as the quadratic \code{lm(y ~ x + I(x^2))} can be used.}
  \item{coef.model}{Defaults to the coefficients of the \code{model}
    argument.  Other intercept and slope coefficients for a straight
    line (for example, \code{c(3,5)}) can be entered to illustrate
    the sense in which they are not "least squares".}
  \item{alt.function}{Any function of a single argument can be placed
    here.\cr
    For example, \code{alt.function=function(x) {3 + 2*x + 3*x^2}}.\cr
    All coefficients must be specified.}
  \item{main, xlab, ylab}{arguments to \code{plot}.}
  \item{jitter.x}{logical.  If \code{TRUE}, the x is jittered before
    plotting.  Jittering is often helpful when there are multiple
    y-values at the same level of x.}
  \item{resid.plot}{If \code{FALSE}, then do not plot the residuals.
    If \code{"square"}, then call \code{resid.squares} to plot the
    squared residuals.   If \code{TRUE} (or anything else),
    then call \code{resid.squares} to plot
    straight lines for the  residuals.}
    \item{points.yhat}{logical.  If \code{TRUE}, the predicted values
      are plotted.}
  \item{\dots}{other arguments.}
  \item{length.x.set}{number of points used to plot the predicted values.}
  \item{x.name}{If the \code{model} argument used a different name for
  the independent variable, you might need to specify it.}
  \item{pch}{Plotting character for the observed points.}
  \item{pch.yhat}{Plotting character for the fitted points.}
  \item{cex.yhat}{\code{cex} for the fitted points.}
  \item{err}{The default \code{-1} suppresses warnings about out of bound
  points.}
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Smith, W. and Gonick, L. (1993).
\emph{The Cartoon Guide to Statistics}.
HarperCollins.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
  This plot is designed as a pedagogical example for introductory courses.
  When \code{resid.plot=="square"}, then we actually see the set of squares
  for which the sum of their areas is minimized by the method of "least squares".
}
\seealso{\code{\link{resid.squares}}}
\examples{
data(hardness)

## linear and quadratic regressions
hardness.lin.lm  <- lm(hardness ~ density,                data=hardness)
hardness.quad.lm <- lm(hardness ~ density + I(density^2), data=hardness)

anova(hardness.quad.lm)  ## quadratic term has very low p-value

par(mfrow=c(1,2))

regr1.plot(hardness$density, hardness$hardness,
           resid.plot="square",
           main="squared residuals for linear fit",
           xlab="density", ylab="hardness",
           points.yhat=FALSE,
           xlim=c(20,95), ylim=c(0,3400))

regr1.plot(hardness$density, hardness$hardness,
           model=hardness.quad.lm,
           resid.plot="square",
           main="squared residuals for quadratic fit",
           xlab="density", ylab="hardness",
           points.yhat=FALSE,
           xlim=c(20,95), ylim=c(0,3400))

par(mfrow=c(1,1))
}
\keyword{models}
\keyword{regression}
