\name{xysplom}
\alias{xysplom}
\alias{xysplom.formula}
\alias{xysplom.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{scatterplot matrix with potentially different sets of variables
  on the rows and columns.}
\description{
scatterplot matrix with potentially different sets of variables
on the rows and columns.  The slope or regression coefficient for
simple least squares regression can
be displayed in the strip label for each panel.
}
\usage{
xysplom(x, ...)

\method{xysplom}{formula}(x, data = sys.parent(), na.action = na.pass, ...)

\method{xysplom}{default}(x, y=x, group, relation="free",
        x.relation=relation, y.relation=relation,
        xlim.in=NULL, ylim.in=NULL,
        corr=FALSE, beta=FALSE, abline=corr||beta, digits=3,
        x.between=NULL, y.between=NULL,
        between.in=list(x=x.between, y=y.between),
        scales.in=list(
          x=list(relation=x.relation, alternating=FALSE),
          y=list(relation=y.relation, alternating=FALSE)),
        strip.in=strip.xysplom,
        pch=16, cex=.75,
        panel.input=panel.xysplom, ...,
        cartesian=TRUE,
        plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{In the \code{"formula"} method, a formula.  In the
    \code{"default"} method, a data.frame.
  Any variables that are used in a formula with \code{+} should be
  numeric.  Factors are not rejected, but their levels will be
  combined strangely.
}
  \item{\dots}{other arguments to \code{xyplot}.}
z
%% xysplom.formula
  \item{data}{data.frame}
  \item{na.action}{See
 %% R
#ifndef S-Plus
   \code{\link[stats]{na.action}}.
#endif
#ifdef S-Plus
    \code{\link[splus]{na.exclude}}.
#endif
    Defaults to \code{na.pass}
    because \code{xyplot} does sensible things with missing data.}

%% xysplom.default
  \item{y}{In the \code{"default"} method, a data.frame with the
    same number of rows as the data.frame in \code{x}.}
  \item{group}{In the \code{"default"} method, a data.frame with the
    same number of rows as the data.frame in \code{x}.}
  \item{relation, x.relation, y.relation,scales.in}{Alternate ways to get to the
  \code{scales(relation=)} arguments to \code{xyplot}.}
  \item{xlim.in, ylim.in}{Alternate ways to get to the
  \code{scales(limits=)} arguments to \code{xyplot}.}
  \item{corr, beta}{Display the correlation and/or the regression
    coefficient for \code{lm(y ~ x)} for each panel in an additional
    strip label.}
  \item{abline}{logical.  If \code{TRUE}, draw the least squares
    regression line within each panel.  By default the abline is \code{FALSE}
    unless at least one of \code{corr} or \code{beta} is \code{TRUE}.}
  \item{digits}{number of significant digits for the correlation coefficient.}
  \item{x.between, y.between, between.in}{Alternate ways to get to the
    \code{between=} argument to \code{xyplot}.}
  \item{strip.in}{strip function that knows how to handle the \code{corr} and
  \code{beta} displays.}
  \item{pch, cex}{arguments to \code{xyplot}}
  \item{panel.input}{panel function used by \code{xyplot} within each
    panel.  When \code{abline==FALSE}, the default panel function calls
    \code{panel.xyplot}.  When \code{abline==TRUE}, the default panel
    function calls \code{panel.xyplot} and\cr
    \code{panel.abline(lm(y~x, na.action=na.exclude))}.\cr
    Note that we use
    \code{na.action=na.exclude} inside \code{lm}.}
  \item{cartesian}{When \code{cartesian==TRUE}, the cartesian product
  of the left-hand side number of variables and the right-hand side
  number of variables defines the number of panels in the display.
  When \code{cartesian==FALSE}, each variable in the left-hand side is
  paired with the variable in the corresponding position in the right-hand side
  and only those pairs are plotted.  Both sides must have the same number of
  variables.}
  \item{plot}{Defaults to \code{TRUE}.  See details.}
}
\details{
  The argument \code{plot=TRUE} is the normal setting and then the
  function returns a \code{"trellis"} object.  When the argument
  \code{plot=FALSE}, the function returns the argument list that would
  otherwise be sent to \code{xyplot}.  This list is interesting when the
  function \code{xysplom} was designed because the function works
  by restructuring the input data and running \code{xyplot} on the
  restructured data.
}
\value{
  When \code{plot=TRUE} (the normal setting),
  the \code{"trellis"} object containing the graph.\cr
  When \code{plot=FALSE}, the restructured data that must be sent to the
  \code{xyplot} function.
}
\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
#ifndef S-Plus
  \code{\link[lattice]{xyplot}}
#endif
#ifdef S-Plus
  \code{xyplot}
#endif
  in R.
}
\examples{

## xysplom syntax options

tmp <- data.frame(y=rnorm(12), x=rnorm(12), z=rnorm(12), w=rnorm(12),
                  g=factor(rep(1:2,c(6,6))))
tmp2 <- tmp[,1:4]

xysplom(y + w ~ x , data=tmp, corr=TRUE, beta=TRUE, cartesian=FALSE, layout=c(1,2))

xysplom(y + x ~ z | g, data=tmp, layout=c(2,2))
xysplom(y + x ~ z | g, data=tmp, cartesian=FALSE)

xysplom(w + y ~ x + z, data=tmp)
xysplom(w + y ~ x + z | g, data=tmp, layout=c(2,4))
xysplom(w + y ~ x + z | g, data=tmp, cartesian=FALSE)

\dontrun{
## xyplot in R has many similar capabilities with xysplom
if.R(r=
       xyplot(w + z ~ x + y, data=tmp, outer=TRUE)
     ,s=
       {}
    )
}

}
\keyword{hplot}% at least one, from doc/KEYWORDS
