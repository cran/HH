\name{defunct}
\alias{HH-defunct}
\alias{anova.mean}
\alias{vcov.sufficient}
\alias{aov.sufficient}
\alias{print.glht.mmc.multicomp}
\alias{coef.arima.HH}
\alias{glht.mmc}
\alias{odds.ratio}
\alias{plot.odds.ratio}
\alias{persp.plane}
\alias{persp.plane}
\alias{persp.floor}
\alias{persp.back.wall.x}
\alias{persp.back.wall.y}
\alias{persp.setup}
\alias{plot.hov}
\alias{plot.hov.bf}
\alias{plot.matchMMC}
\alias{seqplot.forecast}
\alias{lm.case}

\title{Defunct Functions in Package 'HH'}

\description{ The function names listed here are no longer part of the HH
  package.  Their task has been assigned to different function names.
}

\usage{
anova.mean(...)               ## anovaMean
vcov.sufficient(...)          ## vcovSufficient
aov.sufficient(...)           ## aovSufficient
print.glht.mmc.multicomp(...) ## print.mmc.multicomp
coef.arima.HH(...)            ## coefArimaHH
glht.mmc(...)                 ## mmc
odds.ratio(...)               ## OddsRatio
plot.odds.ratio(...)          ## plotOddsRatio
persp.plane(...)              ## perspPlane
persp.floor(...)              ## perspFloor
persp.back.wall.x(...)        ## perspBack.wall.x
persp.back.wall.y(...)        ## perspBack.wall.y
persp.setup(...)              ## not used in R, S-Plus only
plot.hov(...)                 ## hovPlot
plot.hov.bf(...)              ## hovPlot.bf
plot.matchMMC(...)            ## plotMatchMMC
seqplot.forecast(...)         ## seqplotForecast
lm.case(...)                  ## case.lm
}
\arguments{
\item{\dots}{other arguments.}
}
\details{Some of these function names have been replaced by using them
as methods.  Some have had their spelling changed to remove the '.'
character.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{misc }
