\name{aovSufficient}
\alias{aovSufficient}
%% \alias{aov.sufficient} %% old name
\alias{vcovSufficient}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analysis of variance from sufficient statistics for groups.}
\description{
  Analysis of variance from sufficient statistics for groups.
  For each group, we need the factor level, the response mean, the
  within-group standard deviation, and the sample size.
  The correct ANOVA table is produced.  The residuals are fake.
  The generic \code{vcov} and \code{summary.lm} don't work for the
  variance of the regression coefficients in this case.
  Use \code{vcovSufficient}.
}
\usage{
aovSufficient(formula, data = NULL,
              projections = FALSE, qr = TRUE, contrasts = NULL,
              weights = data$n, sd = data$s,
              ...)

vcovSufficient(object, ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{formula, data, projections, qr, contrasts, \dots}{See
%% R
#ifndef S-Plus
    \code{\link[stats]{aov}}.
#endif
#ifdef S-Plus
    \code{\link[splus]{aov}}.
#endif
  }
  \item{weights}{See
%% R
#ifndef S-Plus
    \code{\link[stats]{lm}}.
#endif
#ifdef S-Plus
    \code{\link[splus]{lm}}.
#endif
  }
  \item{sd}{vector of within-group standard deviations.}
  \item{object}{\code{"aov"} object constructed by
    \code{aovSufficient}.  It also works with regular \code{aov} objects.
  }
}
\value{For \code{aovSufficient}, an object of class c("aov", "lm").
For \code{vcovSufficient}, a function that returns the covariance
  matrix of the regression coefficients.}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
The residuals are fake.  They are all identical and equal to the MLE
  standard error (\code{sqrt(SumSq.res/df.tot)}).  They give the right
  ANOVA table.  They may cause confusion or warnings in other programs.
  The standard errors and t-tests of the coefficients are not calculated
  by \code{summary.lm}.
  Using the \code{aov} object from \code{aovSufficient} in \code{glht}
  requires the \code{vcov.} and \code{df} arguments.

}
\seealso{\code{\link{MMC}} and
%% R
#ifndef S-Plus
  \code{\link[stats]{aov}}.
#endif
#ifdef S-Plus
  \code{\link[splus]{aov}}.
#endif
}
\examples{
## This example is from Hsu and Peruggia

## This is the R version
## See ?mmc.mean for S-Plus

if.R(s={},
r={

data(pulmonary)
pulmonary
pulmonary.aov <- aovSufficient(FVC ~ smoker,
                                data=pulmonary)
summary(pulmonary.aov)

\dontrun{
pulmonary.mmc <- mmc(pulmonary.aov,
                     linfct=mcp(smoker="Tukey"),
                     df=pulmonary.aov$df.residual,
                     vcov.=vcovSufficient)
old.omd <- par(omd=c(.0,.95,0,1))
plot(pulmonary.mmc, ry=c(2.5, 3.4), x.offset=.05)

## tiebreaker plot, with contrasts ordered to match MMC plot,
## with all contrasts forced positive and with names also reversed,
## and with matched x-scale.
plotMatchMMC(pulmonary.mmc$mca)

## orthogonal contrasts
pulm.lmat <- cbind("npnl-mh"=c( 1, 1, 1, 1,-2,-2), ## not.much vs lots
                   "n-pnl"  =c( 3,-1,-1,-1, 0, 0), ## none vs light
                   "p-nl"   =c( 0, 2,-1,-1, 0, 0), ## {} arbitrary 2 df
                   "n-l"    =c( 0, 0, 1,-1, 0, 0), ## {} for 3 types of light
                   "m-h"    =c( 0, 0, 0, 0, 1,-1)) ## moderate vs heavy
dimnames(pulm.lmat)[[1]] <- row.names(pulmonary)
pulm.lmat

pulmonary.mmc <- mmc(pulmonary.aov,
                     linfct=mcp(smoker="Tukey"),
                     df=pulmonary.aov$df.residual,
                     vcov.=vcovSufficient,
                     focus.lmat=pulm.lmat)

plot(pulmonary.mmc, ry=c(2.5, 3.4), x.offset=.05)
plotMatchMMC(pulmonary.mmc$lmat, col.signif='blue')

## pairwise and orthogonal contrasts on the same plot
plot(pulmonary.mmc, ry=c(2.5, 3.4), x.offset=.05,
     print.mca=TRUE, print.lmat=TRUE)
par(old.omd)
}
})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
