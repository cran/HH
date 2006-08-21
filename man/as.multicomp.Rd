\name{as.multicomp}
\alias{as.multicomp}
\alias{as.multicomp.hmtest}
%\alias{as.mmc.multicomp}
\alias{print.mmc.multicomp}
\alias{plot.multicomp}
\alias{as.hmtest}
\alias{as.hmtest.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Support functions in R for MMC (mean--mean multiple comparisons) plots.}
\description{
  MMC plots: functions used to interface the \code{simint} in R to the MMC
  functions designed with S-Plus \code{multicomp} notation.  These are
  all internal functions that the user doesn't see.
}
\usage{
as.multicomp(x, ...)

as.multicomp.hmtest(x,
                    group="group",
                    focus=x$FNAMES$mainF,
                    ylabel=x$FNAMES$response,
                    lmcall=x$call,
                    means,
                    lmat=t(x$cmatrix),
                    lmat.rows=seq(along=dimnames(lmat)[[1]])[-1],
                    aov.mca,
                    ...)

print.mmc.multicomp(x, ...)

plot.multicomp(x, ...)

as.hmtest(x, ...)

as.hmtest.multicomp(x, eps=.001, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{"hmtest"} object for \code{as.multicomp}.
  A \code{"mmc.multicomp"} object for \code{print.mmc.multicomp}.
  A \code{"multicomp"} object for \code{as.hmtest} and \code{plot.multicomp}.}
  \item{\dots}{other arguments.}

  %as.multicomp.hmtest
  \item{group}{Not currently used}
  \item{focus}{See \code{whichf} in \code{\link[multcomp]{simint}}}
  \item{ylabel}{response variable name on the graph.}
  \item{lmcall}{Not currently used}
  \item{means}{means of the response variable on the \code{focus} factor.}
  \item{lmat, lmat.rows}{\code{\link{simint.mmc}}}
  \item{aov.mca}{\code{"aov"} object for the same model formula.}

  %as.hmtest.multicomp
  \item{eps}{absolute error tolerance as double.  See \code{\link[multcomp]{simint}}.}
}
\value{
\code{as.multicomp} is a generic function to change its argument to a
\code{"multicomp"} object.

\code{as.multicomp.hmtest} changes an \code{"hmtest"} object to a
\code{"multicomp"} object.

%\code{as.mmc.multicomp} is a generic function to change its argument to a
%\code{"mmc.multicomp"} object.

\code{print.mmc.multicomp} prints a \code{"mmc.multicomp"} object.
}

\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006, accepted).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different libraries.
  MMC plots in R are based on  \code{\link[multcomp]{simint}}.
  MMC plots in S-Plus are based on  \code{\link[splus]{multicomp}}.
  The MMC plot is the same in both systems.  The details of gettting the
  plot differ.
  }
\seealso{\code{\link{mmc}}, \code{\link{simint.mmc}},
  \code{\link[multcomp]{simint}} in R,
  \code{\link[splus]{multicomp}} in S-Plus.}
\keyword{ dplot }

