\name{hov}
\alias{hov}
\alias{hov.bf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Homogeneity of Variance}
\description{
  Oneway analysis of variance makes the assumption that the variances of
  the groups are equal.
Brown and Forsyth, 1974 present the recommended test of this assumption.
The Brown and Forsyth test statistic is the \eqn{F} statistic resulting
from an ordinary one-way analysis of variance on the
absolute deviations from the median.
}
\usage{
hov(x, data = sys.parent(), method = "bf") ## x is a formula

## users will normally use the formula above and will not call the
## method directly.
hov.bf(x, group,  ## x is the response variable
       y.name = deparse(substitute(x)),
       group.name = deparse(substitute(group)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Formula appropriate for oneway anova in \code{hov}.
  Response variable in \code{hov.bf}.}
  \item{data}{ data.frame }
  \item{method}{Character string defining method.  At this time the only
  recognized method is \code{"bf"} for the Brown--Forsyth method.
}

  \item{group}{ factor. }
  \item{y.name}{ name of response variable,
    defaults to variable name in formula. }
  \item{group.name}{name of factor, defaults to variable name in
    formula. }
  }
\value{
\code{"htest"} object for the hov test.
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
\seealso{ \code{\link[stats]{aov}},  \code{\link{hovPlot}}}
\examples{
data(turkey)

hov(wt.gain ~ diet, data=turkey)
hovPlot(wt.gain ~ diet, data=turkey)
}
\keyword{ models }
