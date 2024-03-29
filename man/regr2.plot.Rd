\name{regr2.plot}
\alias{regr2.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{3D plot of z against x and y,
with regression plane fit and display of squared residuals.}
\description{
3D plot of z against x and y,
with regression plane fit and display of squared residuals.
}
\usage{
regr2.plot(x, y, z,
           main.in="put a useful title here",
           resid.plot=FALSE,
           plot.base.plane=TRUE,
           plot.back.planes=TRUE,
           plot.base.points=FALSE,
           eye=NULL,                   ## S-Plus
           theta=0, phi=15, r=sqrt(3), ticktype="detailed", ## R
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y,z}{See
%% R
#ifndef S-Plus
    \code{\link[graphics]{persp}}.
#endif
#ifdef S-Plus
    \code{\link[splus]{persp}}.
#endif
  }
  \item{main.in}{\code{main} title for plot.}
  \item{resid.plot}{Argument to \code{\link{resid.squares}}.}
  \item{plot.base.plane, plot.back.planes, plot.base.points}{Should
    these items be plotted?}
  \item{eye}{S-Plus only.  See
%% R
#ifndef S-Plus
  \code{persp}.
#endif
#ifdef S-Plus
  \code{\link[splus]{persp}}.
#endif
  }
  \item{theta, phi, r, ticktype}{R only.  See
%% R
#ifndef S-Plus
    \code{\link[graphics]{persp}}.
#endif
#ifdef S-Plus
  \code{persp}.
#endif
  }
  \item{\dots}{Other arguments to \code{persp}.}
}
\value{"Viewing Transformation" for projecting 3D coordinates (x,y,z)
  into the 2D plane.  See \code{\link[graphics]{persp}} for details.
}
\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course with Examples in R}.
Second Edition.
Springer-Verlag, New York.
\url{https://link.springer.com/book/10.1007/978-1-4939-2122-5}

Smith, W. and Gonick, L. (1993).
\emph{The Cartoon Guide to Statistics}.
HarperCollins.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
  This plot is designed as a pedagogical example for introductory
  courses.  When \code{resid.plot=="square"}, then we actually see the
  set of squares for which the sum of their areas is minimized by the
  method of "least squares".  The demo called in the examples section
  shows the geometry of regression coefficients, the change in predicted
  y when x1 is changed one unit holding all other x variables constant.
}
\seealso{\code{\link{resid.squares}}, \code{\link{regr1.plot}}, \code{\link[graphics]{persp}}}
\examples{
data(fat)
regr2.plot(fat[,"abdomin"], xlab="abdomin",
           fat[,"biceps"],  ylab="biceps",
           fat[,"bodyfat"], zlab="bodyfat",
           resid.plot="square",
           eye=c(335.5, 115.65, 171.9),   ## used only in S-Plus
           theta=140, phi=35, r=sqrt(15), ## used only in R
           box=is.R(),
           plot.back.planes=FALSE,
           main="Least-squares with two X-variables")

\dontrun{
  demo("regr2", package="HH", ask=FALSE)
  ## run the file manually to see the individual steps.
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
