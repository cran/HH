\name{residVSfitted}
\alias{residVSfitted}
\alias{scaleLocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw plots of resid ~ y.hat and sqrt(abs(resid)) ~ y.hat
}
\description{
  Draw plots of \code{resid ~ y.hat} and \code{sqrt(abs(resid)) ~ y.hat}.
  This is a pair of \pkg{lattice} functions that duplicate the first and third
  panels of \code{stats:::plot.lm}.
}
\usage{
residVSfitted(linearmodel, groups = (e >= 0), ...)
scaleLocation(linearmodel, groups = (e >= 0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{linearmodel}{
\code{"lm"} object.
}
  \item{groups}{
This is the standard \code{groups} argument for
\code{\link[lattice]{xyplot}}.
The default value is one symbol and color for positive residuals and a
different symbol and color for negative residuals.
}
  \item{\dots}{
Additional arguments to \code{xyplot}.
}
}
\value{
\code{"trellis"} object.
}
\author{
Richard M. Heiberger <rmh@temple.edu>
}
\examples{
data(fat)
fat.lm <- lm(bodyfat ~ abdomin, data=fat)

A <- residVSfitted(fat.lm, pch=c(25,24),
                   fill=trellis.par.get("superpose.symbol")$col[1:2])
B <- scaleLocation(fat.lm, pch=c(25,24),
                   fill=trellis.par.get("superpose.symbol")$col[1:2])
BA <- c("Scale-Location"=B,
        "Residuals vs Fitted"=update(A, scales=list(y=list(at=-100, alternating=3))),
        layout=c(1,2))
BA

BAu <-
  update(BA,
         ylab=c(B$ylab, A$ylab),
         ylab.right=c(B$ylab.right, A$ylab.right),
         xlab.top=NULL,
         between=list(y=1),
         par.settings=list(layout.widths=list(ylab.right=6))
         )

C <- diagQQ(fat.lm)

D <- diagplot5new(fat.lm)

print(BAu, split=c(1,1,2,1), more=TRUE)

print(update(c("Normal Q-Q"=C), xlab.top=NULL, strip=TRUE),
      ## split=c(2,1,2,2),
      position=c(.5, .54, 1, 1),  ## .54 is function of device and size
      more=TRUE)

print(update(D, xlab.top=NULL,
             strip=strip.custom(factor.levels=D$xlab.top),
             par.strip.text=list(lines=1.3)),
      ## split=c(2,2,2,2),
      position=c(.5, 0, 1, .57),  ## .57 is function of device and size
      more=FALSE)
## the .54 and .57 work nicely with the default quartz window on Mac OS X.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
