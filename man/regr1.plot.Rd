\name{regr1.plot}
\alias{regr1.plot}
\title{plot x and y,
with optional straight line fit and display of squared residuals}
\description{
plot x and y,
with optional straight line fit and display of squared residuals
}
\usage{
regr1.plot(x, y, model=lm(y~x), coef.model=coef(model),
           main="put a useful title here",
           xlab=deparse(substitute(x)),
           ylab=deparse(substitute(y)),
           jitter.x=FALSE,
           resid.plot=FALSE,
           points.yhat=TRUE,
           ..., length.x.set=51,
           err=-1)
}
\arguments{
  \item{x}{x variable}
  \item{y}{y variable}
  \item{model}{Defaults to the simple linear model \code{lm(y ~ x)}.
    Any linear model object with one x
    variable, such as the quadratic \code{lm(y ~ x + I(x^2))} can be used.}
  \item{coef.model}{Defaults to the coefficients of the \code{model}
    argument.  Other coefficients can be entered to illustrate the
    sense in which they are not "least squares".
  }
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
  \item{err}{}
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
hardness <- read.table(hh("datasets/hardness.dat"), header=TRUE)

## linear and quadratic regressions
hardness.lin.lm  <- lm(hardness ~ density,                data=hardness)
hardness.quad.lm <- lm(hardness ~ density + I(density^2), data=hardness)

anova(hardness.quad.lm)  ## qyadratic term has very low p-value

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
