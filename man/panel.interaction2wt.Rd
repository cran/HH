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
panel.interaction2wt(x, y, subscripts,
                     responselab, trace.values,
                     factor.levels, factor.position,
                     fun = mean,
                     se,
                     ...,
                     box.ratio,
                     simple=FALSE,
                     simple.offset,
                     simple.scale,
                     simple.pch,
                     data.x,
                     col.by.row=TRUE,
                     key.in=NULL)

strip.interaction2wt(which.given, which.panel, var.name,
                     factor.levels, shingle.intervals,
                     strip.names = c(TRUE, TRUE), style = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \code{panel.interaction2wt} arguments:
  \item{x}{levels of x-factor}
  \item{y}{ Summary value of response variable at each level of
    x- and trace-factors. }
  \item{subscripts}{used to get the right set of response values for the
    summary statistics on the off-diagonals}
  \item{responselab}{Character name of response variable, defaults to
    the name of the response variable.}
  \item{trace.values}{levels of trace-factor}
  \item{fun}{Summary function, defaults to \code{mean}}
  \item{se}{standard errors to be passed to \code{panel.intxplot}.
    Missing, logical, or a numeric vector.
    If  \code{se} is  missing or \code{FALSE}, or if \code{simple} is
    \code{FALSE}, then standard errors are not plotted.
    If \code{TRUE}, the standard errors are calculated from the
    sufficient statistics for each group as the group's standard deviation
    divided by the square root of the group's observation
    count.  If a numeric vector, it is evaluated in the environment of
    the sufficient statistics.
  },
  \item{box.ratio}{passed to \code{panel.bwplot.intermediate.hh}},
  \item{\dots}{extra arguments, primarily color,
    to be passed to \code{panel.bwplot.intermediate.hh}}
  \item{key.in}{S-Plus only.  Arguments to be passed through to the \code{key} for the
    trace-factor in each row of the display.
  The most likely argument is \code{x}, which is needed if the key is
  not correctly placed.  Use, for example, \code{key.in=list(x=-3.5)}
  where the units are the units of the left column of panels and the
  value is the location where the left border of the key should be placed.
}
  \item{factor.position}{\code{"position"} attribute of factor.}
  \item{simple}{logical.  If \code{TRUE}, then simple effects are to be displayed.}
  \item{simple.offset, simple.scale}{named list of offset and scale for the
    response and trace factors.\cr
    See \code{\link{interaction.positioned}} for their use.}
  \item{simple.pch}{Named list containing plotting characters for each
    level of one or more of the factors.  It is used only when
    \code{simple==TRUE}.  If the list contains plotting characters for
    both factors in the formula, the item associated with the first
    factor in the formula is used.  If the \code{simple.pch} is missing,
    then the integers for the levels of the first factor in the model
    formula are used (For model formula \code{y ~ A*B}, the values are
    \code{seq(along=levels(A))}).  }
  \item{data.x}{\code{data.frame} containing factors from the input data.frame}
  \item{col.by.row}{logical.  If \code{TRUE} (the default), simple
effects plots color the simple effects on the main diagonals in the same
color as the trace levels in their row.  If \code{FALSE}, then simple
effects are colored to match the x levels in their column.}

  \code{strip.interaction2wt} arguments
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
\dontrun{
tmp <- data.frame(y=rnorm(48),
                  A=factor(rep(1:2, 24)),
                  B=factor(rep(rep(1:3, each=2), 8)),
                  C=factor(rep(rep(1:4, each=6), 2)))
interaction2wt(y ~ A+B+C, data=tmp,
               key.in=list(x=-3), ## key.in is ignored by R
               xlim=c(.4, 4.5))
interaction2wt(y ~ B+C, data=tmp, key.in=list(x=-2), xlim=c(.4, 4.5))
position(tmp$B) <- c(1, 2.4, 3.8)
interaction2wt(y ~ B+C, data=tmp, key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ B+C, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ C+B, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ B+C, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               simple.pch=list(C=c(16,17,18,19)),
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ C+B, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               simple.pch=list(C=c(16,17,18,19)),
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ C+B, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               simple.pch=list(A=c(1:2), B=c(3:5), C=c(16,17,18,19)),
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ C+B, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               simple.pch=list(A=c(1:2)),
               key.in=list(x=-2), xlim=c(.4, 4.5))
interaction2wt(y ~ B+C, data=tmp, simple=TRUE,
               simple.scale=list(B=.18, C=.27),  box.ratio=.2,
               simple.pch=list(B=c(16,17,18)),
               key.in=list(x=-2), xlim=c(.4, 4.5),
               se=TRUE)
}
}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ design }% __ONLY ONE__ keyword per line
