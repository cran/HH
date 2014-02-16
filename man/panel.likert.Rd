\name{panel.likert}
\alias{panel.likert}
\alias{panel.barchart2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel functions for likert that include a stackWidth argument}
\description{
 \code{panel.barchart2} is based on \code{panel.barchart}\cr
 The changes are\cr
 * the heights in each horizontal stacked bar are constant.\cr
 * the widths in each vertical stacked bar are constant.\cr
 * the panel.barchart heights and widths are based on the box.width argument.\cr
 * the panel.barchart2 heights and widths when \code{stack==TRUE} are also based
   on the new \code{stackWidth} argument.

   \code{panel.likert} calls \code{panel.barchart2}

 scaling of \code{stackWidth}:\cr
 \code{stackWidth <- stackWidth/mean(stackWidth) ## and maybe smaller with another /2}
}
\usage{
panel.barchart2(x, y, box.ratio = 1, box.width = box.ratio/(1 + box.ratio),
    horizontal = TRUE, origin = NULL, reference = TRUE, stack = FALSE,
    groups = NULL,
    col = if (is.null(groups)) plot.polygon$col else superpose.polygon$col,
    border = if (is.null(groups)) plot.polygon$border else superpose.polygon$border,
    lty = if (is.null(groups)) plot.polygon$lty else superpose.polygon$lty,
    lwd = if (is.null(groups)) plot.polygon$lwd else superpose.polygon$lwd,
    ..., identifier = "barchart",
          stackWidth=NULL)

panel.likert(..., horizontal=TRUE, reference.line.col="gray65")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, box.ratio, box.width, horizontal, origin, reference,
    stack, groups, col}{See \code{\link[lattice]{panel.barchart}}.}
  \item{border, lty, lwd, identifier}{See \code{\link[lattice]{panel.barchart}}.}
  \item{\dots}{Extra arguments, if any, for \code{panel.barchart}.}
  \item{stackWidth}{Heights in each horizontal stacked bar, when
    stack=TRUE, are constant and specified by this argument.  We
    recommend starting with\cr
    \code{stackWidth <- stackWidth/mean(stackWidth)} and adjusting as seems appropriate.}
  \item{reference.line.col}{See \code{\link{likert}}.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{likert}}}
\keyword{dplot }
