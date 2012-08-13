\name{X.residuals}
\alias{X.residuals}
\alias{X.residuals.default}
\alias{X.residuals.formula}
\alias{X.residuals.lm}
\title{ Residuals from the regression of each column of a
  data.frame against all the other columns.}
\description{
Calculate the residuals from the regression of each column of a
  data.frame against all the other columns.
}
\usage{
X.residuals(x, ...)

\method{X.residuals}{default}(x, y.name, na.action = na.exclude, ...) ## x is a data.frame

\method{X.residuals}{formula}(x, data, na.action = na.exclude, ...)   ## x is a formula

\method{X.residuals}{lm}(x, na.action = na.exclude, ...)  ## x is a "lm" object computed with x=TRUE
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{data.frame}, or \code{formula}, or \code{lm} object
    computed with \code{x=TRUE}.}
  \item{na.action}{See
#ifndef S-Plus
   \code{\link[stats]{na.action}}.
#endif
#ifdef S-Plus
    \code{\link[splus]{na.exclude}}.
#endif
  }
  \item{\dots}{additional arguments. }

  \item{y.name}{Name of Y-variable to be excluded from the computations.}

  \item{data}{A data frame in which the variables specified in the
    formula will be found. If missing, the variables are searched for in
    the standard way. }
}
\value{
Data.frame of residuals, one column from each regression.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
  \code{\link[stats]{lm}},
#endif
#ifdef S-Plus
  \code{\link[splus]{lm}},
#endif
  \code{\link{vif}},
  \code{\link{case.lm}}.}
\examples{
data(usair)
usair$lnSO2 <- log(usair$SO2)
usair$lnmfg <- log(usair$mfgfirms)
usair$lnpopn <- log(usair$popn)

usair.lm <- lm(lnSO2 ~ temp + lnmfg + wind + precip, data=usair, x=TRUE)

X.residuals(usair.lm)  ## the lm object must be computed with x=TRUE

X.residuals(lnSO2 ~ temp + lnmfg + wind + precip, data=usair)

X.residuals(usair)

X.residuals(usair, y.name="lnSO2")
}
\keyword{regression}
