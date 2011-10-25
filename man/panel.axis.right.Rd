\name{panel.axis.right}
\alias{panel.axis.right}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Right-justify right-axis tick labels.
}
\description{
\code{panel.axis.right} is almost identical to
\code{\link[lattice]{panel.axis}}.
The only difference is that \code{panel.axis.right} right
justifies right axis tick labels.
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
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side,at,labels,draw.labels,check.overlap,outside,ticks,
    half,which.half,tck,rot,text.col,text.alpha,text.cex,text.font,
    text.fontfamily,text.fontface,text.lineheight,line.col,
    line.lty,line.lwd,line.alpha}{See \code{\link[lattice]{panel.axis}}}
}
\author{
Deepayan Sarkar \code{Deepayan.Sarkar@R-project.org} wrote
\code{panel.axis}.
David Winsemius wrote the modification \code{panel.axis.right}.
Richard Heiberger \code{rmh@temple.edu} is maintaining this
distribution of the \code{panel.axis.right} function.
}

\seealso{
\code{\link[lattice]{panel.axis}}
}
\keyword{hplot}
