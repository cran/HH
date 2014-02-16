\name{ladder}
\alias{ladder}
\alias{ladder3}
\alias{ladder.f}
\alias{ladder.fstar}
\alias{strip.ladder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw a "ladder of powers" plot, plotting each of several powers
  of y against the same powers of x.}
\description{
 Draw a "ladder of powers" plot, plotting each of several powers
 of y against the same powers of x.  The powers are

 \code{result <- data.frame(-1/x, -1/sqrt(x), log(x), sqrt(x), x, x^2)}

 \code{names(result) <- c(-1, -.5, 0, .5, 1, 2)}
}
\usage{
ladder(formula.in, data=sys.parent(),
       main.in="Ladders of Powers",
       panel.in=panel.cartesian,
       xlab=deparse(formula.in[[3]]),
       ylab=deparse(formula.in[[2]]),
       scales=list(alternating=if.R(s=TRUE, r=FALSE),
         labels=FALSE, ticks=FALSE, cex=.6),
       par.strip.text=list(cex=.6),
       cex=.5, pch=16, between=list(x=.3, y=.3),
       dsx=xlab,
       dsy=ylab,
       ladder.function=ladder.f,
       strip.number=if.R(r=2, s=1),
       strip.names,
       strip.style=1,
       strip,
       oma=c(0,0,0,0),  ## S-Plus
       axis3.line=.61,
       layout=c(length(tmp$x.power), length(tmp$y.power)),
       axis.key.padding = 10, ## R right axis
       key.axis.padding = 10, ## R top axis
       useOuter=TRUE, ## R useOuterStrips(combineLimits(result))
       ...)

ladder3(x, y,
        dsx=deparse(substitute(x)),
        dsy=deparse(substitute(y)),
        ladder.function=ladder.f)

ladder.f(x)

ladder.fstar(x)

strip.ladder(which.given,
             which.panel,
             var.name,
             factor.levels,
             shingle.intervals,
             par.strip.text=trellis.par.get("add.text"),
             strip.names=c(TRUE,TRUE),
             style=1,
             ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
% ladder
  \item{formula.in}{A formula with exactly one variable on each side.}
  \item{data}{data.frame}
  \item{main.in}{\code{main} title for \code{xyplot}}
  \item{panel.in}{\code{panel.cartesian} has many arguments in addition
             to the arguments in \code{panel.xyplot}.  Any replacement
             panel function must have those argument names, even if it
             doesn't do anything with them.}
  \item{xlab, ylab}{Trellis arguments,  default to right- and left-sides
             of the \code{formula.in}.}
  \item{strip}{Strip function. Our default is \code{strip.ladder}
             (see below).  The other viable argument value is \code{FALSE}.}
  \item{cex, pch, between, scales, layout}{arguments for \code{xyplot}.}
  \item{dsx, dsy}{Names to be used as level names in
    \code{ladder.function} for the generated factor distinguishing the
    powers.  They default to \code{xlab, ylab}.  For long variable names,
    an abbreviated name here will decrease clutter in the ladder of powers
    plot.  These names are not visible in the plot when
    \code{strip=FALSE}.}
  \item{ladder.function}{function to use to create data.frame of powers
    of input variable.}
  \item{strip.number}{Number of strip labels in each panel of the display.
    0: no strip labels; 1: one strip label of the form \code{y^p ~ x^q};
    2: two strip labels of the form \code{ylab: y^p} and \code{xlab: x^q},
    where \code{p} and \code{q} are the powers returned by \code{ladders};
  \code{y} and \code{x} are the arguments \code{dsy} and \code{dsx}.}
  \item{useOuter}{logical, defaults to \code{TRUE}. In R, this implies
    that \code{strip.number} is forced to \code{2} and that the
    resulting \code{"trellis"} object will be sent through\cr
    \code{useOuterStrips(combineLimits(result))}.\cr
    This argument is ignored by S-Plus.}
  \item{strip.style}{\code{style} argument to \code{strip}.}
  \item{oma}{argument to \code{par} in S-Plus.}
  \item{\dots}{other arguments to \code{xyplot}.}
  \item{axis3.line}{extra space to make the top axis align with the top
  of the top row of panels.  Trial and error to choose a good value.}
  \item{axis.key.padding}{Extra space on right of set of panels in R.}
  \item{key.axis.padding}{Extra space on top of set of panels in R.}

% ladder3
  \item{x, y}{variables.}

% ladder.f ladder.fstar

% strip.ladder
\item{which.given, which.panel, var.name, factor.levels,
  shingle.intervals, par.strip.text}{See
%% R
#ifndef S-Plus
  \code{\link[lattice]{strip.default}}.
#endif
#ifdef S-Plus
  \code{\link[trellis]{strip.default}}.
#endif
}
\item{strip.names, style}{We always print the \code{strip.names}
  in \code{style=1}.  Multicolored styles are too busy.}
}
\details{

  The \code{ladder} function uses \code{panel.cartesian} which is
  defined differently in R (using grid graphics) and S-Plus (using
  traditional graphics).  Therefore the fine control over appearance
  uses different arguments or different values for the same arguments.

}
\value{
\code{ladder} returns a \code{"trellis"} object.

The functions \code{ladder.fstar} and \code{ladder.f} take an input
vector \code{x} of non-negative values and construct a data.frame by
taking the input to the powers \code{c(-1, -.5, 0, .5, 1, 2)}, one
column per power.  \code{ladder.f} uses the simple powers and
\code{ladder.fstar} uses the scaled Box--Cox transformation.
\tabular{llr}{
\code{ladder.fstar}        \tab \code{ladder.fstar}         \tab notation \cr
\code{(x^p - 1)/p}         \tab \code{(x^p - 1)/p}          \tab    p     \cr
\code{(1/x - 1)/(-1)}      \tab \code{(1/x - 1)/(-1)}       \tab   -1.0   \cr
\code{(1/sqrt(x)-1)/(-.5)} \tab \code{(1/sqrt(x)-1)/(-.5)}  \tab   -0.5   \cr
\code{log(x)}              \tab \code{log(x)}               \tab    0.0   \cr
\code{((sqrt(x)-1)/.5)}    \tab \code{((sqrt(x)-1)/.5)}     \tab    0.5   \cr
\code{x-1}                 \tab \code{x-1}                  \tab    1.0   \cr
\code{(x^2 - 1)/2}         \tab \code{(x^2 - 1)/2}          \tab    2.0   \cr
       }


\code{ladder3} takes two vectors as arguments.  It returns a
\code{data.frame} with five columns:

  \code{X, Y}: data to be plotted.  The column \code{X} contains the data
    from the input \code{x} taken to all the powers and aligned with the
    similarly expanded column \code{Y}.

  \code{x, y}: symbolic labeling of the power corresponding to \code{X,Y}.

  \code{group}: result from pasting the labels in \code{x, y} with \code{*}
    between them.
}

\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Hoaglin, D.~C., Mosteller, F., and Tukey, J.~W., editors (1983).
\emph{Understanding Robust and Exploratory Data Analysis}.
Wiley.

Box, G. E.~P. and Cox, D.~R. (1964).
An analysis of transformations.
\emph{J. Royal Statist Soc B}, 26:211--252.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{panel.cartesian}}}
\examples{
data(tv)

## default
## R: outer strip labels
## S-Plus: one strip label per panel (too full for this example, see below)
ladder(life.exp ~ ppl.per.phys, data=tv,
       main="Ladder of Powers for Life Expectancy and People per Physician")

\dontrun{
## one strip label
if.R(r=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=1, useOuter=FALSE,
              dsx="ppp", dsy="le"),
     s=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=1,
              dsx="ppp", dsy="le")) ## S-Plus default

## two strip labels
if.R(r=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=2, useOuter=FALSE),
     s=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=2,
              axis3.line=1.2))

## outer strip labels
if.R(r=ladder(life.exp ~ ppl.per.phys, data=tv, useOuter=TRUE), ## R default
     s={}) ## S-Plus not available

## no strip labels (probably silly, but possible)
if.R(r=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=0, useOuter=FALSE),
     s=ladder(life.exp ~ ppl.per.phys, data=tv, strip.number=0,
              axis3.line=0))
}
}
\keyword{hplot}
\keyword{dplot}
\keyword{manip}
