\name{useOuterScales}
\alias{useOuterScales}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Put scales for axes only on the bottom and left panels of a
  lattice display, and give fine control over the placement of strips
}
\description{
Update a multi-panel "trellis" object so that scales for axes are
displayed only on the bottom and left boundaries when printed, instead
of in every panel as is usual.  This function succeeds even when xlim
across columns and ylim across rows are not identical.  Multiple options
are available for strip labels.  The default for strip labels is similar
to \code{\link[latticeExtra]{useOuterStrips}}.  Additional options
include outerStrips for each panel and interchanged row and column strip
locations.  This is only meaningful when there are exactly two
conditioning variables.
}
\usage{
useOuterScales(x,
           axis.xlab.padding=4,
           ylab.axis.padding=3,
           strip,
           strip.left,
           layout.widths.strip.left=.5,
           layout.heights.strip=.5,
           x.ticks=is.numeric(x$x.limits),
           y.ticks= is.numeric(x$y.limits) +
             if (!missing(strip.left) &&   ## FALSE
                 is.logical(strip.left) && ## explicitly stated
                 !strip.left ) 0
             else 2.5,
           inner=FALSE,
           interchangeRC=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{"trellis"}
}
\item{ylab.axis.padding, axis.xlab.padding,
  layout.heights.strip, layout.widths.strip.left}{
  These values are passed to the \code{par.settings}:
  \preformatted{
    layout.widths=list(ylab.axis.padding=ylab.axis.padding,
                       strip.left=layout.widths.strip.left),
    layout.heights=list(axis.xlab.padding=axis.xlab.padding,
                        strip=layout.heights.strip)
}
See \code{\link[lattice]{trellis.par.get}}, and the \code{par.settings} section of
\code{\link[lattice]{xyplot}}.
}
  \item{strip, strip.left}{
\code{useOuterScales} controls the strip labels by assigning appropriate
functions for these two arguments.  The functions used by
\code{useOuterScales} are described in
\code{\link{strip.useOuterStrips.first}}.  \code{useOuterScales} uses
the values of its \code{strip}, \code{strip.left}, \code{inner},
\code{interchangeRC}, and \code{x$as.table} arguments to determine which
functions to assign.  The default values place the columns strip labels
at the top of the top row of panels and the row strip labels at the left
of the left column of panels.  See the Examples section for the full set of
possibilities that are provided.
}
  \item{x.ticks, y.ticks}{
\code{x.ticks} is used as the \code{ticks} argument to
\code{\link[lattice]{panel.axis}} for the \code{"bottom"} axis.  \code{y.ticks}
is used as the \code{ticks} argument to \code{\link[lattice]{panel.axis}} for the
\code{"left"} axis.  \code{y.ticks} needs to be larger when the left
strip is present because the tick and label
are partially overwritten by the left strip.  When the
\code{left.strip=FALSE}, then we need to make the \code{y.ticks} smaller.
}
  \item{inner}{
Logical with default \code{FALSE}, meaning that the strip labels are
displayed only on the top row and left column of the array of
panels. When \code{TRUE}, the strip labels are displayed on the top and
left of every panel.
}
  \item{interchangeRC}{
Logical with default \code{FALSE}.  When \code{TRUE}, the column labels
appear on the left strip of the panels, and the row labels appear on
the top of the panels.  \code{TRUE} is only meaningful when \code{inner=TRUE}.
}
}
\details{
   \code{useOuterScales} modifies a \code{"trellis"} object with
   \code{length(dim(x)) == 2} so that when plotted,
   scales appear on only the top and left panels of the array of panels.
   Strips appear as specified, by default
     on the top and left boundaries of the panel layout.

     If the original \code{"trellis"} object \code{x} includes non-default
     \code{strip} and \code{strip.left} arguments, they will be ignored.  To
     provide customized strip behaviour, specify the custom strip
     functions directly as arguments to \code{useOuterStrips}.
}
\value{
    An object of class \code{"trellis"}; essentially the same as \code{x}, but
     with certain properties modified.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[latticeExtra]{useOuterStrips}}, \code{\link[lattice]{strip.default}}
}
\examples{
OuterScalesData <- data.frame(y=1:16,
                              AA=rep(factor(letters[1:8]), 2),
                              BB=rep(factor(LETTERS[12:13]), each=8),
                              CC=rep(factor(rep(LETTERS[9:11], times=c(3,1,4))), 2))
OuterScalesData

BC0 <- barchart(AA ~ y | BB * CC, data=OuterScalesData,
               origin=0,
               scales=list(x=list(limits=c(0,16.5)),
                           y=list(relation="free")),
               between=list(x=1, y=1),
               main="0. barchart")
\dontrun{
BC0
}
BC1 <- update(
  resizePanels(BC0, h=c(3,1,4)),
  main="1. resizePanels")
BC1

BC2 <- update(
  useOuterStrips(BC1),
  main="2. useOuterStrips") ## package:latticeExtra
BC2

BC3 <- update(
  useOuterScales(BC1),
  main="3. useOuterScales")
BC3

\dontrun{
BC4 <- update(
  useOuterScales(BC1),
  ylab="ABC",
  main="4. useOuterScales, ylab")
BC4

BC5 <- update(
  useOuterScales(update(BC1, as.table=TRUE)),
  main="5. useOuterScales, as.table")
BC5

try(useOuterScales(BC1, interchangeRC=TRUE)) ## incompatible options
}

BC6 <- update(
  useOuterScales(BC1, inner=TRUE),
  main="6. useOuterScales, inner")
BC6

\dontrun{
BC7 <- update(
  useOuterScales(BC1, inner=TRUE, interchangeRC=TRUE),
  main="7. useOuterScales, inner, interchangeRC")
BC7

BC8 <- update(
  useOuterScales(BC1, strip=FALSE),
  xlab.top=c("L","M"),
  main="8. useOuterScales, strip=FALSE, xlab.top")
BC8

BC9 <- update(
  useOuterScales(BC1, strip=strip.default),
  main="9. useOuterScales, strip=strip.default")
BC9

try(print(useOuterScales(BC1, strip=date))) ## date is not a valid strip function

BC10 <- update(
  useOuterScales(BC1, strip.left=FALSE),
  ylab=c("I","J","K"),
  main="10. useOuterScales, strip.left=FALSE, ylab")
BC10

BC11 <- update(
  useOuterScales(BC1, strip.left=strip.default),
  main="11. useOuterScales, strip.left=strip.default")
BC11

try(print(useOuterScales(BC1, strip.left=date))) ## date is not a valid strip function

BC12 <- update(
  useOuterScales(BC1,
                 inner=TRUE, interchangeRC=TRUE, strip.left=FALSE),
  xlab.top=c("L","M"),
  main=
"12. useOuterScales, inner, \n interchangeRC=TRUE, strip.left=FALSE, \n xlab.top, strip.background",
  par.settings=list(strip.background=list(col="gray98")))
BC12

BC13 <- update(
  useOuterScales(update(BC1, as.table=TRUE),
                 inner=TRUE, interchangeRC=TRUE, strip.left=FALSE),
  xlab.top=c("L","M"),
  main="13. useOuterScales, inner, \n interchangeRC=TRUE, strip.left=FALSE, \n xlab.top, as.table")
BC13

BC14 <- update(
  useOuterScales(BC1,
                 inner=TRUE, strip=FALSE, interchangeRC=TRUE),
  ylab=list(c("I","J","K"), rot=0),
  main="14. useOuterScales, inner, \n strip=FALSE, interchangeRC, \n ylab")
BC14

BC15 <- update(
  useOuterScales(BC1,
                 strip=FALSE, strip.left=FALSE),
  xlab.top=c("L","M"), ylab=list(c("I","J","K"), rot=0),
  main="15. useOuterScales, strip=FALSE, strip.left=FALSE, \n xlab, ylab")
BC15
}

\dontrun{ ## display 16 options for strip labels with outerScales
useOuterScales16 <- tempfile("useOuterScales16", fileext = ".pdf")
pdf(useOuterScales16, height=16, width=21)
print(BC0,  split=c(1,1,4,4), more=TRUE)
print(BC1,  split=c(2,1,4,4), more=TRUE)
print(BC2,  split=c(3,1,4,4), more=TRUE)
print(BC3,  split=c(4,1,4,4), more=TRUE)
print(BC4,  split=c(1,2,4,4), more=TRUE)
print(BC5,  split=c(2,2,4,4), more=TRUE)
print(BC6,  split=c(3,2,4,4), more=TRUE)
print(BC7,  split=c(4,2,4,4), more=TRUE)
print(BC8,  split=c(1,3,4,4), more=TRUE)
print(BC9,  split=c(2,3,4,4), more=TRUE)
print(BC10, split=c(3,3,4,4), more=TRUE)
print(BC11, split=c(4,3,4,4), more=TRUE)
print(BC12, split=c(1,4,4,4), more=TRUE)
print(BC13, split=c(2,4,4,4), more=TRUE)
print(BC14, split=c(3,4,4,4), more=TRUE)
print(BC15, split=c(4,4,4,4), more=FALSE)
dev.off()
}


\dontrun{
## Verify y.ticks default value depends on
##  is.numeric(x$y.limits).
##  and on whether strip.left=FALSE

CB0 <- barchart(y ~ AA | CC * BB, data=OuterScalesData,
                origin=0,
                scales=list(y=list(limits=c(0,16.5)),
                            x=list(relation="free")),
                between=list(x=1, y=1),
                main="CB0. barchart")
CB0

CB1 <- update(
  resizePanels(CB0, w=c(3,1,4)),
  main="CB1. resizePanels")
CB1

CB2 <- update(
  useOuterStrips(CB1),
  main="CB2. useOuterStrips") ## package:latticeExtra
CB2

CB3 <- update(
  useOuterScales(CB1),
  main="CB3. useOuterScales, y.limits is numeric")
CB3

CB4 <- update(
  useOuterScales(CB1, strip.left=FALSE),
  main="CB4. useOuterScales, y.limits is numeric, strip.left=FALSE")
CB4

BC16 <- update(
  useOuterScales(BC1),
  main="BC16. useOuterScales, y.limits is not numeric")
BC16

BC17 <- update(
  useOuterScales(BC1, strip.left=FALSE),
  main="BC17. useOuterScales, y.limits is not numeric, strip.left=FALSE")
BC17

}

\dontrun{
## Verify x.ticks default value depends on
##  is.numeric(x$x.limits).

update(BC3, main="BC3. useOuterScales, x.limits is numeric")

update(CB3, main="CB3. useOuterScales, x.limits is not numeric")

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
  \keyword{ aplot }
