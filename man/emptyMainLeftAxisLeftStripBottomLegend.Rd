\name{emptyMainLeftAxisLeftStripBottomLegend}
\alias{emptyMainLeftAxisLeftStripBottomLegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove main title, left axis tick labels, left strip, bottom legend
from plot and keep the vertical spacing allocated to those items.
}
\description{
Remove main title, left axis tick labels, left strip, bottom legend from
plot and keep the vertical spacing allocated to those items.  This
function is used to prepare a trellis object to be placed next to
another trellis object.  The current object will have much of its
annotation
removed with the intent of sharing annotation with the other object.
This is motivated by the \code{ProfChal} example in \code{\link{plot.likert}}.
}
\usage{
emptyMainLeftAxisLeftStripBottomLegend(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{"trellis"} object.}
}
\details{
We manipulate the items inside the trellis object.
}
\value{
  A \code{"trellis"} object with the stated items replaced by non-printing
  values.  The vertical spacing of the original object is retained.
}
\references{
  The manipulations are similar to those in the
  \code{\link[latticeExtra]{c.trellis}} and related functions in the
  \code{latticeExtra} package.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{plot.likert}}}
\examples{
## This is a small example.
## See ?plot.likert for the complete example including motivation.
##
require(grid)
require(lattice)
require(latticeExtra)
require(HH)

data(ProfChal)

tmp <- data.matrix(ProfChal[,1:5])
rownames(tmp) <- ProfChal$Question
ProfChal.list <- split.data.frame(tmp, ProfChal$Subtable)

Empl <- ProfChal.list[[2]]

pct   <- likert(Empl, as.percent="noRightAxis", xlab="Percent")
pct
count <- likert(Empl, rightAxis=TRUE,
                xlab="Count", ylab.right="Row Count Totals",
                scales=list(x=list(at=c(0, 100, 200))))
count
countEmptied <- HH:::emptyMainLeftAxisLeftStripBottomLegend(count)
countEmptied

tmp <- update(resizePanels(c(pct, countEmptied, y.same=TRUE, layout=c(2,1)), w=c(.8, .2)),
              scales=list(y=list(alternating=3, limits=count$y.limits),
                          x=list(at=list(pct$x.scales$at, count$x.scales$at),
                                 labels=list(pct$x.scales$labels,
                                             count$x.scales$labels))),
              xlab=c(" ", pct$xlab, " ", count$xlab),
              between=list(x=1))
tmp
}
\keyword{hplot}
