\name{panel.interaction2wt}
\alias{panel.interaction2wt}
\alias{strip.interaction2wt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot all main effects and twoway interactions in a multifactor design}
\description{
This is the panel function for \code{interaction2wt}.  The main diagonal
displays boxplots for the main effects of each factor.  The
off-diagonals show the interaction plots for each pair of factors.
The \code{i,j} panel shows the same factors as the \code{j,i} but with
the trace- and x-factor roles interchanged.
}
\usage{
panel.interaction2wt(x, y, subscripts, responselab,
                     trace.values, factor.levels, fun = mean, ...)

strip.interaction2wt(which.given, which.panel, var.name,
                     factor.levels, shingle.intervals,
                     strip.names = c(TRUE, TRUE), style = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \code{panel.interaction2wt arguments}
  \item{x}{levels of x-factor}
  \item{y}{ Summary value of response variable at each level of
    x- and trace-factors. }
  \item{subscripts}{used to get the right set of response values for the
    summary statistics on the off-diagonals}
  \item{responselab}{Character name of response variable, defaults to
    the name of the response variable.}
  \item{trace.values}{levels of trace-factor}
  \item{fun}{Summary function, defaults to \code{mean}}
  \item{\dots}{extra arguments, primarily color,
    to be passed to \code{panel.bwplot.intermediate.hh}}

  \code{strip.interaction2wt arguments}
  \item{which.given, which.panel, var.name, factor.levels,
    shingle.intervals}{see documentation
    for \code{\link[lattice]{strip.default}}}.
  \item{strip.names}{Force \code{strip.names=TRUE}}
  \item{style}{force \code{style=1}}
}
\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
% \note{ ~~further notes~~ 
%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{interaction2wt}},
  \code{\link{panel.bwplot.intermediate.hh}}}
\examples{
%data(vulcan, package="HH")
%interaction2wt(wear ~ filler + pretreat + raw, data=vulcan)
anova(aov(len ~ supp*dose, data=ToothGrowth))
interaction2wt(len ~ supp + dose, data=ToothGrowth)
bwplot(len ~ supp | dose, data=ToothGrowth, layout=c(3,1))
}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ design }% __ONLY ONE__ keyword per line
