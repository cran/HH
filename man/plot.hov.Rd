\name{plot.hov}
\alias{hovPlot}
%% \alias{plot.hov} %% old name
\alias{hovPlot.bf}
\alias{panel.hov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Homogeneity of Variance Plot}
\description{
  Oneway analysis of variance makes the assumption that the variances of
  the groups are equal.
Brown and Forsyth, 1974 present the recommended test of this assumption.
The Brown and Forsyth test statistic is the \eqn{F} statistic resulting
from an ordinary one-way analysis of variance on the
absolute deviations from the median.  The \code{hovPlot} function
graphs the components of the Brown and Forsyth test statistic.
}
\usage{
hovPlot(x, data = sys.parent(), method = "bf",  ## x is a formula
        transpose = TRUE, ...)

## users will normally use the formula above and will not call the
## method directly.
hovPlot.bf(x, group,  ## x is the response variable
           y.name = deparse(substitute(x)),
           group.name = deparse(substitute(group)),
           transpose = TRUE, ...)

## users will normally use the formula above and will not call the
## panel function directly.
panel.hov(..., transpose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Formula appropriate for oneway anova in \code{hovPlot}.
  Response variable in \code{hovPlot.bf}.}
  \item{data}{ data.frame }
  \item{method}{Character string defining method.  At this time the only
  recognized method is "bf" for the Brown-Forsyth method.}
  \item{transpose}{Always \code{TRUE} in R.
    Normally \code{TRUE} in S-Plus to force vertical boxplots.}


  \item{group}{ factor. }
  \item{y.name}{ name of response variable,
    defaults to variable name in formula. }
  \item{group.name}{name of factor, defaults to variable name in formula. }

  \item{\dots}{additional arguments.}
}
\value{
  \code{"trellis"} object with three panels containing boxplots for each
  group: The observed data \code{"y"}, the data with the median
  subtracted \code{"y-med(y)"}, and the absolute deviations from the
  median \code{"abs(y-med(y))"} The Brown and Forsyth test statistic is
  the \eqn{F} statistic resulting from an ordinary one-way analysis of
  variance on the data points in the third panel.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Brown, M.~B. and Forsyth, A.~B. (1974).
\emph{Robust tests for equality of variances}.
\emph{Journal of the American Statistical Association}, 69:364--367.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{ \code{\link[stats]{aov}},  \code{\link{hov}}}
\examples{
data(turkey)

hov(wt.gain ~ diet, data=turkey)
hovPlot(wt.gain ~ diet, data=turkey)
}
\keyword{ models }
\keyword{hplot}
