\name{HH-package}
\alias{HH-package}
\alias{HH}
\docType{package}
\title{
  Support software for
  Statistical Analysis and Data Display
  by
        Richard M. Heiberger and Burt Holland
}
\description{
Support software for \emph{Statistical Analysis and Data
        Display} (Springer, ISBN 0-387-40270-5).  This contemporary
        presentation of statistical methods features extensive use of
        graphical displays for exploring data and for displaying the
        analysis. The authors demonstrate how to analyze data---showing
        code, graphics, and accompanying computer listings---for all the
        methods they cover. They emphasize how to construct and
        interpret graphs, discuss principles of graphical design, and
        show how accompanying traditional tabular results are used to
        confirm the visual impressions derived directly from the
        graphs. Many of the graphical formats are novel and appear
        here for the first time in print. All chapters have exercises.
}
\details{
\tabular{ll}{
Package: \tab HH\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2006-08-21\cr
License: \tab GPL version 2 or newer\cr
}
%~~ An overview of how to use the package, including the most important functions ~~

data display, scatterplot matrix, MMC mean--mean multiple comparison
plots, interaction plots, ANCOVA plots, regression diagnostics, time
series, ARIMA models, boxplots

}
\author{
Richard M. Heiberger

Maintainer: Richard M. Heiberger <rmh@temple.edu>
}
\references{
Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\keyword{ package }
% \keyword{ data display }
% \keyword{ scatterplot matrix }
% \keyword{ multiple comparisons }
% \keyword{ interaction plots }
% \keyword{ ANCOVA plots }
% \keyword{ regression diagnostics }
% \keyword{ time series }
% \keyword{ ARIMA models }
% \keyword{ boxplots }
\keyword{hplot}
\keyword{data}
\keyword{htest}
\keyword{design}
\keyword{ts}
\seealso{
  \code{\link{ancova}},
    \code{\link{ci.plot}},
    \code{\link{interaction2wt}},
    \code{\link{ladder}},
    \code{\link{case.lm}},
    \code{\link{norm.curve}},
    \code{\link{hov}},
    \code{\link{resid.squares}},
    \code{\link{MMC}},
    \code{\link{xysplom}}
}
\examples{
## interaction2wt()
## multicomp.mmc()  ## S-Plus
## mmc()       ## R
## ancova()
## xysplom()
## plot.case()
## bwplot() ## with position
## tsacfplots()     ## at this writing, only S-Plus
## tsdiagplot()     ## at this writing, only S-Plus
}
