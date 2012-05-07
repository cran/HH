\name{vif}
\alias{vif}
\alias{vif.default}
\alias{vif.formula}
\alias{vif.lm}
\title{ Calculate the Variance Inflation Factor }
\description{
The VIF for predictor \eqn{i} is \eqn{1/(1-R_i^2)}, where \eqn{R_i^2} is the
\eqn{R^2} from a regression of predictor \eqn{i} against the remaining
predictors.  
}
\usage{
vif(xx, ...)

\method{vif}{default}(xx, y.name, na.action = na.exclude, ...) ## xx is a data.frame

\method{vif}{formula}(xx, data, na.action = na.exclude, ...)   ## xx is a formula

\method{vif}{lm}(xx, na.action = na.exclude, ...)  ## xx is a "lm" object computed with x=TRUE
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{\code{data.frame}, or \code{formula}, or \code{lm} object
    computed with \code{x=TRUE}.}
  \item{na.action}{See
 %% R
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
\details{
A simple diagnostic of collinearity is the
\emph{variance inflation factor, VIF}
one for each regression coefficient (other than the intercept).
Since the condition of collinearity involves the predictors but not
the response, this measure is a function of the \eqn{X}'s but not of \eqn{Y}.
The VIF for predictor \eqn{i} is \eqn{1/(1-R_i^2)}, where \eqn{R_i^2} is the
\eqn{R^2} from a regression of predictor \eqn{i} against the remaining
predictors.  If \eqn{R_i^2} is close to 1, this means that predictor \eqn{i}
is well explained by a linear function of the remaining predictors,
and, therefore, the presence of predictor \eqn{i} in the model is
redundant.  Values of VIF exceeding 5 are considered evidence of
collinearity: The information carried by a predictor having such a VIF
is contained in a subset of the remaining predictors.  If, however,
all of a model's regression coefficients differ significantly from 0
(\eqn{p}-value \eqn{<} .05), a somewhat larger VIF may be tolerable.  
}
\value{
Vector of VIF values, one for each X-variable.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
  \code{\link[stats]{lm}}.
#endif
#ifdef S-Plus
  \code{\link[splus]{lm}}.
#endif
}
\examples{
data(usair)
if.R(s={usair <- usair}, r={})

usair$lnSO2 <- log(usair$SO2)
usair$lnmfg <- log(usair$mfgfirms)
usair$lnpopn <- log(usair$popn)

usair.lm <- lm(lnSO2 ~ temp + lnmfg + wind + precip, data=usair, x=TRUE)

vif(usair.lm)  ## the lm object must be computed with x=TRUE

vif(lnSO2 ~ temp + lnmfg + wind + precip, data=usair)

vif(usair)

vif(usair, y.name="lnSO2")
}
\keyword{regression}
