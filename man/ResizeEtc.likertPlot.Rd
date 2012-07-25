\name{ResizeEtc.likertPlot}
\alias{ResizeEtc.likertPlot}
\title{Display multiple independent trellis objects, representing
  likert plots, on the same coordinated scale.
}
\description{
This is a method for \code{ResizeEtc} intended for use with
\code{"likert"} plots that allows positive values on the
negative side of the axis.
}
\usage{
\method{ResizeEtc}{likertPlot}(c.list,
                     x,
                     x.pl.nonames,
                     horizontal,
                     ...)
}
\arguments{
  \item{c.list}{combination of two or more trellis objects
    from \code{\link[latticeExtra]{c.trellis}}.  If \code{c.list} has
    names, the names will appear in the strips.}
  \item{x}{
    List of two-dimensional objects with the same columns.  See
    \code{\link{plot.likert.list}} for details.
}
\item{x.pl.nonames}{
  List of \code{"likert"} objects corresponding to the items in argument
  \code{x}.  The items in \code{x.pl.nonames} are unnamed.
}
\item{horizontal}{
  Standard argument for \code{\link{barchart}}.
}
\item{\dots}{Other arguments to \code{\link{ResizeEtc}.}
}
}
\value{
The result is a \code{"trellis"} object.  It is essentially the same
object
returned by \code{ResizeEtc} with possibly adjusted x tick-labels to put
positive labels on the negative axis.  If \code{horizontal==FALSE},
then the possible adjusted labels are the y tick-labels.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link{ResizeEtc}}, \code{\link{likert}}.
}
\keyword{hplot}
