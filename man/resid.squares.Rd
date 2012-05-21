\name{resid.squares}
\alias{resid.squares}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot squared residuals in inches to match the y-dimension}
\description{
  plot squared residuals in inches to match the y-dimension
}
\usage{
resid.squares(x, y, y.hat, resid.plot = "square", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x values}
  \item{y}{observed y values}
  \item{y.hat}{predicted y values}
  \item{resid.plot}{If \code{"square"}, then plot the
    squared residuals.   If \code{TRUE} (or anything else),
    then plot straight lines for the  residuals.}
  \item{\dots}{Other graphics arguments.}
}
\details{
  The goal is to get real squares on the screen or paper.
  The trick is to play games with the aspect ratio.
  We find the number of inches that each vertical residual occupies.
  We then find the number of x-units that corresponds to, and plot
  a rectangle with height=height in the y-data units and with width=the
  number of x-units that we just calculated.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{regr1.plot}}}
\examples{
data(hardness)

hardness.lin.lm  <- lm(hardness ~ density, data=hardness)

plot(hardness ~ density, data=hardness, xlim=c(22,73), ylim=c(0,3400))
abline(hardness.lin.lm)
resid.squares(hardness$density, hardness$hardness,
              predict(hardness.lin.lm))

plot(hardness ~ density, data=hardness, xlim=c(22,73), ylim=c(0,3400))
abline(hardness.lin.lm)
resid.squares(hardness$density, hardness$hardness,
              predict(hardness.lin.lm), resid.plot = "line")
}
\keyword{models}
\keyword{regression}
