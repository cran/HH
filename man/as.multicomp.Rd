\name{as.multicomp}
\alias{as.multicomp}
\alias{as.multicomp.glht}
\alias{as.glht}
\alias{as.glht.multicomp}
\alias{print.mmc.multicomp}
%% \alias{print.glht.mmc.multicomp}
\alias{print.mmc.multicomp}
\alias{print.multicomp}
\alias{print.multicomp.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Support functions in R for MMC (mean--mean multiple comparisons) plots.}
\description{
  MMC plots: In R, functions used to interface the \code{glht} in R to the MMC
  functions designed with S-Plus \code{multicomp} notation.  These are
  all internal functions that the user doesn't see.
}
\usage{
\method{print}{mmc.multicomp}(x, ..., width.cutoff=options()$width-5)

\method{print}{multicomp}(x, ...)

## print.multicomp.hh(x, digits = 4, ..., height=T) ## S-Plus only

\method{print}{multicomp.hh}(x, ...) ## R only

%% print.glht.mmc.multicomp(x, ...) ## R.  yes, spell it out.

as.multicomp(x, ...)

\method{as.multicomp}{glht}(x,       ## glht object
           focus,   ## currently required
           ylabel=deparse(terms(x$model)[[2]]),
           means=model.tables(x$model, type="means",
                              cterm=focus)$tables[[focus]],
           height,
           lmat=t(x$linfct),
           lmat.rows=lmatRows(x, focus),
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           contrasts.none=FALSE,
           level=0.95,
           calpha=NULL,
           method=x$type,
           df,
           vcov.,
           ...
           )

as.glht(x, ...)

\method{as.glht}{multicomp}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{"glht"} object for \code{as.multicomp}.
    A \code{"mmc.multicomp"} object for \code{print.mmc.multicomp}.
    %% and \code{print.glht.mmc.multicomp}.
    A \code{"multicomp"} object for \code{as.glht} and \code{print.multicomp}.}
  \item{\dots}{other arguments.}

  %as.multicomp.glht
  \item{focus}{name of focus factor.}
  \item{ylabel}{response variable name on the graph.}
  \item{means}{means of the response variable on the \code{focus} factor.}
  \item{lmat, lmat.rows}{\code{\link{mmc}}}
  \item{lmat.scale.abs2}{logical, almost always \code{TRUE}.  If it is
    not \code{TRUE}, then the contrasts will not be properly placed
    on the MMC plot.}
  \item{estimate.sign}{numeric. 1: force all contrasts to be positive by
  reversing negative contrasts. $-1$: force all contrasts to be negative by
  reversing positive contrasts.  Leave contrasts as they are constructed
  by \code{glht}.}
  \item{order.contrasts, height}{logical.  If \code{TRUE}, order contrasts by
  \code{height} (see \code{\link[HH]{mmc}}).}
  \item{contrasts.none}{logical.  This is an internal detail.  The
    ``contrasts'' for the group means are not real contrasts in the
    sense they don't compare anything.  \code{mmc.glht} sets this
    argument to \code{TRUE} for the \code{none} component.
  }
  \item{level}{Confidence level.  Defaults to 0.95.}
  \item{calpha}{R only.  User-specified critical point.
    See
%% R
#ifndef S-Plus
    \code{\link[multcomp]{confint.glht}}.
#endif
#ifdef S-Plus
    \code{confint.glht}.
#endif
  }
  \item{df, vcov.}{R only.  Arguments forwarded through \code{glht} to
%% R
#ifndef S-Plus
    \code{\link[multcomp]{modelparm}}.
#endif
#ifdef S-Plus
    \code{modelparm}.
#endif
  }
  \item{method}{R only.  See \code{type} in
%% R
#ifndef S-Plus
    \code{\link[multcomp]{confint.glht}}.
#endif
#ifdef S-Plus
    \code{confint.glht}.
#endif
  }
  \item{width.cutoff}{See \code{\link[base]{deparse}}.}
}
\details{
The \code{mmc.multicomp} \code{print}
method displays the confidence intervals and heights on the
MMC plot for each component of the \code{mmc.multicomp} object.

\code{print.multicomp} displays the confidence intervals and heights for
a single component.

% \code{print.glht.mmc.multicomp(x, ...)} uses \code{print.glht} on each
% component of a \code{mmc.multicomp} object and therefore prints only
% the estimates of the comparisons.
}
\value{
\code{as.multicomp} is a generic function to change its argument to a
\code{"multicomp"} object.

\code{as.multicomp.glht} changes an \code{"glht"} object to a
\code{"multicomp"} object.  If the model component of the argument \code{"x"}
is an \code{"aov"} object then the standard error is taken from the
\code{anova(x$model)} table, otherwise from the \code{summary(x)}.
With a large number of levels for the focus factor, the
\code{summary(x)}
function is exceedingly slow (80 minutes for 30 levels on 1.5GHz Windows
XP).
For the same example, the \code{anova(x$model)} takes a fraction of
a second.}

\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different libraries.
  MMC plots in R are based on
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus
  \code{glht}.
#endif
  MMC plots in S-Plus are based on
%% R
#ifndef S-Plus
  \code{multicomp}.
#endif
#ifdef S-Plus
  \code{\link[splus]{multicomp}}.
#endif
  The MMC plot is the same in both systems.  The details of gettting the
  plot differ.
  }
\seealso{\code{\link{mmc}},
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus
  \code{\link[splus]{multicomp}}.
#endif
  }
\keyword{ dplot }
