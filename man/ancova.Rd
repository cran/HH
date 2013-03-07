\name{ancova}
\alias{ancova}
\alias{analysis of covariance}
\alias{covariance}
\alias{anova.ancova}
\alias{predict.ancova}
\alias{print.ancova}
\alias{model.frame.ancova}
\alias{summary.ancova}
\alias{plot.ancova}
\alias{coef.ancova}
%\alias{coefficients.ancova}
\alias{panel.ancova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute and plot oneway analysis of covariance}
\description{
  Compute and plot oneway analysis of covariance.
  The result object is an \code{ancova} object which consists of
  an ordinary \code{aov} object with an additional \code{trellis}
  attribute.  The
  \code{trellis} attribute is a \code{trellis} object consisting of
  a series of plots of \code{y ~ x}.  The left set of panels is
  conditioned on the levels of the factor \code{groups}.  The right
  panel is a superpose of all the groups.
}
\usage{
ancova(formula, data.in = NULL, ...,
       x, groups, transpose = FALSE,
       display.plot.command = FALSE,
       superpose.level.name = "superpose",
       ignore.groups = FALSE, ignore.groups.name = "ignore.groups",
       blocks, blocks.pch = letters[seq(levels(blocks))],
       layout, between, main)

panel.ancova(x, y, subscripts, groups,
 transpose = FALSE, ...,
 coef, contrasts, classes,
 ignore.groups, blocks, blocks.pch, blocks.cex)

## The following are ancova methods for generic functions.
\method{anova}{ancova}(object, ...)

\method{predict}{ancova}(object, ...)

\method{print}{ancova}(x, ...) ## prints the anova(x) and the trellis attribute

\method{model.frame}{ancova}(formula, ...)

\method{summary}{ancova}(object, ...)

\method{plot}{ancova}(x, y, ...) ## standard lm plot.  y is always ignored.

\method{coef}{ancova}(object, ...)

%\method{coefficients}{ancova}(object, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula specifying the model.}
  \item{data.in}{A data frame in which the variables specified in the
    formula will be found. If missing, the variables are searched for in
    the standard way.}
  \item{\dots}{Arguments to be passed to \code{aov}, such as \code{subset}
    or \code{na.action}.}
  \item{x}{Covariate in \code{ancova}, needed for plotting when the
    formula does not include \code{x}.
  \code{"aov"} object in \code{print.ancova}, to match the argument of
  the \code{print} generic function.
  Variable to plotted in \code{"panel.ancova"}.
}
  \item{groups}{Factor.  Needed for plotting when the formula does not
    include \code{groups} after the conditioning bar \code{"|"}.}
  \item{transpose}{S-Plus: The axes in each panel of the plot are
    transposed.  The analysis
    is identical, just the axes displaying it have been interchanged.
  R: no effect.}
\item{display.plot.command}{The default setting is usually what the user
wants.  The alternate value \code{TRUE} prints on the console the
command that draws the graph.  This is strictly for debugging the
\code{ancova} command.}
  \item{superpose.level.name}{Name used in strip label for superposed panel.}
  \item{ignore.groups}{When \code{TRUE}, an additional panel showing all
  groups together with a common regression line is displayed.}
  \item{ignore.groups.name}{Name used in strip label for
    \code{ignore.groups} panel.}
  \item{blocks}{Additional factor used to label points in the panels.}
  \item{blocks.pch}{Alternate set of labels used when a \code{blocks}
    factor is specified.}
  \item{blocks.cex}{Alternate set of \code{cex} used when a \code{blocks}
    factor is specified.}
  \item{layout}{The layout of multiple panels.  The default is a single
    row.  See details.}
  \item{between}{Space between the panels for the individual group
    levels and the superpose panel including all groups.}
  \item{main}{Character with a main header title to be done on the top
    of each page.}
  \item{y,subscripts}{In \code{"panel.ancova"},
%% R
#ifndef S-Plus
see \code{\link[lattice]{panel.xyplot}}.
#endif
#ifdef S-Plus
see both
    \code{\link[trellis]{xyplot}} and
    \code{\link[trellis]{trellis.args}}.
#endif
  }
  \item{object}{An \code{"aov"}} object.  The functions using this
  argument are methods for the similarly named generic functions.
\item{coef, contrasts, classes}{Internal variables used to communicate between
  \code{ancova} and \code{panel.ancova}.  They keep track
  of the constant or different slopes and intercepts in each
  panel of the plot.}
}

\details{ The \code{ancova} function does two things.  It passes its
  arguments directly to the \code{aov} function and returns the entire
  \code{aov} object.  It also rearranges the data and formula in its
  argument and passes that to the \code{xyplot} function.  The
  \code{trellis} attribute is a \code{trellis} object consisting of
  a series of plots of \code{y ~ x}.  The left set of panels is
  conditioned on the levels of the factor \code{groups}.  The right
  panel is a superpose of all the groups. }

\value{ The result object is an \code{ancova} object which consists of
  an ordinary \code{aov} object with an additional \code{trellis}
  attribute.  The default print method is to print both the \code{anova}
  of the object and the \code{trellis} attribute.  }

\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
% \note{ ~~further notes~~
%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
\code{\link{ancova-class}}
\code{\link[stats]{aov}}
\code{\link[lattice]{xyplot}}
}
\examples{
data(hotdog)

## y ~ x                     ## constant line across all groups
ancova(Sodium ~ Calories,     data=hotdog, groups=Type)

## y ~ a                     ## different horizontal line in each group
ancova(Sodium ~            Type, data=hotdog, x=Calories)

## This is the usual usage
## y ~ x + a  or  y ~ a + x  ## constant slope, different intercepts
ancova(Sodium ~ Calories + Type, data=hotdog)
ancova(Sodium ~ Type + Calories, data=hotdog)

## y ~ x * a  or  y ~ a * x  ## different slopes, and different intercepts
ancova(Sodium ~ Calories * Type, data=hotdog)
ancova(Sodium ~ Type * Calories, data=hotdog)

## y ~ a * x ## save the object and print the trellis graph
hotdog.ancova <- ancova(Sodium ~ Type * Calories, data=hotdog)
attr(hotdog.ancova, "trellis")


## label points in the panels by the value of the block factor
data(apple)
ancova(yield ~ treat + pre, data=apple, blocks=block)

## Please see
##      demo("ancova")
## for a composite graph illustrating the four models listed above.
}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\keyword{hplot}
\keyword{dplot}
\keyword{models}
\keyword{regression}
