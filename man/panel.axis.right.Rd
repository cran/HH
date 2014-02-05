\name{panel.axis.right}
\alias{panel.axis.right}
\alias{axis.RightAdjustRight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Right-justify right-axis tick labels.
}
\description{
\code{panel.axis.right} is almost identical to
\code{\link[lattice]{panel.axis}}.
\code{axis.RightAdjustRight} is almost identical to \code{\link[lattice]{axis.default}}.
The only difference is that these functions right-justify right-axis tick labels.
}
\usage{
panel.axis.right(side = c("bottom", "left", "top", "right"),
                 at = pretty(scale.range),
                 labels = TRUE, draw.labels = TRUE,
                 check.overlap = FALSE, outside = FALSE, ticks = TRUE,
                 half = !outside,
                 which.half = switch(side, bottom = "lower",
                                     left = "upper", top = "upper",
                                     right = "lower"),
                 tck = as.numeric(ticks),
                 rot = if (is.logical(labels)) 0 else c(90, 0),
                 text.col = axis.text$col, text.alpha = axis.text$alpha,
                 text.cex = axis.text$cex, text.font = axis.text$font,
                 text.fontfamily = axis.text$fontfamily,
                 text.fontface = axis.text$fontface,
                 text.lineheight = axis.text$lineheight,
                 line.col = axis.line$col, line.lty = axis.line$lty,
                 line.lwd = axis.line$lwd, line.alpha = axis.line$alpha)

axis.RightAdjustRight(side = c("top", "bottom", "left", "right"),
                      scales, components, as.table,
                      labels = c("default", "yes", "no"),
                      ticks = c("default", "yes", "no"),
                      ...,
                      %% prefix = lattice:::lattice.getStatus("current.prefix"))
                      prefix = lattice.lattice.getStatus("current.prefix"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side, at, labels, draw.labels, check.overlap, outside, ticks,
    half, which.half}{
    See \code{\link[lattice]{panel.axis}} and
    \code{\link[lattice]{axis.default}}
    }
  \item{tck, rot, text.col, text.alpha, text.cex, text.font, text.fontfamily}{
    See \code{\link[lattice]{panel.axis}} and
    \code{\link[lattice]{axis.default}}
    }
  \item{text.fontface, text.lineheight, line.col, line.lty, line.lwd, line.alpha}{
    See \code{\link[lattice]{panel.axis}} and
    \code{\link[lattice]{axis.default}}
    }
  \item{scales, components, as.table, \dots, prefix}{See \code{\link[lattice]{axis.default}}}
}
\author{
Deepayan Sarkar \code{Deepayan.Sarkar@R-project.org} wrote
\code{panel.axis} and \code{axis.default}.
David Winsemius wrote the modification \code{panel.axis.right}.
Richard Heiberger \code{rmh@temple.edu} wrote the modification \code{axis.RightAdjustRight}.
Richard Heiberger is maintaining this
distribution of both functions.
}

\seealso{
\code{\link[lattice]{panel.axis}}
}
\keyword{hplot}
