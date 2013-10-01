\name{cp.calc}
\alias{cp.calc}
\alias{print.cp.object}
\alias{[.cp.object}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rearranges and improves the legibility of the output from
  the stepwise function in S-Plus.  }
\description{
  Rearranges and improves the legibility of the output from
  the
%% R
#ifndef S-Plus
  \code{stepwise}
#endif
#ifdef S-Plus
  \code{\link[splus]{stepwise}}
#endif
  function in S-Plus.
  The output can be used for the Cp plot.
  \code{cp.calc} works only in S-Plus.
  Use
%% R
#ifndef S-Plus
  \code{\link[leaps]{regsubsets}}
#endif
#ifdef S-Plus
  \code{regsubsets}
#endif
  in R.  The example below works in
  both languages.
}
\usage{
cp.calc(sw, data, y.name)

\method{print}{cp.object}(x, ...)

\method{[}{cp.object}(x, ..., drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sw}{Output from the S-Plus
%% R
#ifndef S-Plus
  \code{stepwise}
#endif
#ifdef S-Plus
  \code{\link[splus]{stepwise}}
#endif
 function.}
  \item{data}{Dataset name from which \code{"sw"} was calculated.}
  \item{y.name}{Name of response variable for which \code{"sw"} was calculated.}
  \item{x}{Object of class \code{"cp.object"}. }
  \item{\dots}{Additional arguments to \code{"["} or \code{"print"}.}
  \item{drop}{Argument to the \code{print} function.}
}
\value{\code{"cp.object"}, which is a data.frame containing information
about each model that was attempted with additional
attributes:
 \code{tss} total sum of squares,
 \code{n} number of observations,
 \code{y.name} response variable,
 \code{full.i} row name of full model.  The columns are
\item{p}{number of parameters in the model}
\item{cp}{Cp statistic}
\item{aic}{AIC statistic}
\item{rss}{Residual sum of squares}
\item{r2}{\eqn{R^2}}
\item{r2.adj}{Adjusted \eqn{R^2}}
\item{xvars}{X variables}
\item{sw.names}{Model name produced by \code{stepwise}.}
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## This example is from Section 9.15 of Heiberger and Holland (2004).
data(usair)
if.R(s={usair <- usair}, r={})

splom(~usair, main="U.S. Air Pollution Data with SO2 response", cex=.5)
## export.eps(hh("regb/figure/regb.f1.usair.eps"))

usair$lnSO2 <- log(usair$SO2)
usair$lnmfg <- log(usair$mfgfirms)
usair$lnpopn <- log(usair$popn)

usair[1:3,]   ## lnSO2 is in position 8, SO2 is in position 1
              ## lnmfg is in position 9, lnpopn is in position 10

splom(~usair[, c(8,2,9,10,5:7)],
              main="U.S. Air Pollution Data with 3 log-transformed variables",
              cex=.5)
## export.eps(hh("regb/figure/regb.f2.usair.eps"))

if.R(s={
  usair.step <- stepwise(y=usair$lnSO2,
                         x=usair[, c(2,9,10,5:7)],
                         method="exhaustive",
                         plot=FALSE, nbest=2)
  ## print for pedagogical purposes only.  The plot of cp ~ p is more useful.
  ## The line with rss=1e35 is a stepwise() bug, that we reported to S-Plus.
  print(usair.step, digits=4)
  usair.cp <- cp.calc(usair.step, usair, "lnSO2")
  ## print for pedagogical purposes only.  The plot of cp ~ p is more useful.
  usair.cp
  tmp <- (usair.cp$cp <= 10)
  usair.cp[tmp,]

  old.par <- par(mar=par()$mar+c(0,1,0,0))
  tmp <- (usair.cp$cp <= 10)
  plot(cp ~ p, data=usair.cp[tmp,], ylim=c(0,10), type="n", cex=1.3)
  abline(b=1)
  text(x=usair.cp$p[tmp], y=usair.cp$cp[tmp],
       row.names(usair.cp)[tmp], cex=1.3)
  title(main="Cp plot for usair.dat, Cp<10")
  par(old.par)
## export.eps(hh("regb/figure/regb.f3.usair.eps"))
},r={
  usair.regsubset <- leaps::regsubsets(lnSO2~lnmfg+lnpopn+precip+raindays+temp+wind,
                                       data=usair, nbest=2)
  usair.subsets.Summary <- summaryHH(usair.regsubset)
  tmp <- (usair.subsets.Summary$cp <= 10)
  usair.subsets.Summary[tmp,]
  plot(usair.subsets.Summary[tmp,], statistic='cp', legend=FALSE)

  usair.lm7 <- lm.regsubsets(usair.regsubset, 7)
  anova(usair.lm7)
  summary(usair.lm7)
})

vif(lnSO2 ~ temp + lnmfg + lnpopn + wind + precip + raindays, data=usair)

vif(lnSO2 ~ temp + lnmfg + wind + precip, data=usair)

usair.lm <- lm(lnSO2 ~ temp + lnmfg + wind + precip, data=usair)
anova(usair.lm)
summary(usair.lm, corr=FALSE)
}
\keyword{regression}
