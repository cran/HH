\name{HH-package}
\alias{HH-package}
\alias{HH}
\docType{package}
\title{
\packageTitle{HH}
}
\description{
\packageDescription{HH}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{HH}
\packageIndices{HH}

%~~ An overview of how to use the package, including the most important functions ~~

data display, scatterplot matrix, (MMC Mean--mean Multiple Comparison)
plots, interaction plots, ANCOVA plots, regression diagnostics, time
series, ARIMA models, boxplots

}
\author{
\packageAuthor{HH}

Maintainer: \packageMaintainer{HH}
}
\references{
  Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in R}, Second Edition.
 Springer Texts in Statistics. Springer.
 ISBN 978-1-4939-2121-8.

  Heiberger, Richard M. and Holland, Burt (2004).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}, First Edition.
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
\keyword{shiny}
\seealso{
  \code{\link{ancovaplot}},
  \code{\link{ci.plot}},
  \code{\link{interaction2wt}},
  \code{\link{ladder}},
  \code{\link{case.lm}},
  \code{\link{NTplot}} for Normal and \eqn{t} plots,
  \code{\link{hov}},
  \code{\link{resid.squares}},
  \code{\link{MMC}},
  %% \code{\link{xysplom}},
  \code{\link{AEdotplot}},
  \code{\link{likert}},
  \code{\link{tsacfplots}},
  \code{\link{tsdiagplot}}

  \code{demo(package="HH")}
}

\examples{
## In addition to the examples for each function,
## there are seven interactive shiny apps in the HH package:
\dontrun{
if (interactive())   NTplot(mean0=0, mean1=1, shiny=TRUE)
if (interactive())   shiny::runApp(system.file("shiny/bivariateNormal", package="HH"))
if (interactive())   shiny::runApp(system.file("shiny/bivariateNormalScatterplot", package="HH"))
if (interactive())   shiny::runApp(system.file("shiny/PopulationPyramid", package="HH"))
if (interactive())   shiny.CIplot(height = "auto")
if (interactive())   shiny::runApp(system.file("shiny/AEdotplot", package="HH"))
if (interactive())   shiny::runApp(system.file("shiny/likert", package="HH"))
}
}
