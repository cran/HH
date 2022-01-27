\name{likertWeighted}
\alias{likertWeighted}
\title{
Special case wrapper for likert() when multiple columns are to have the
same bar thicknesses.  Uses formula.
}
\description{
Special case wrapper for likert() when multiple columns are to have the
same bar thicknesses.  Uses formula.  Each row of the data is placed in
its own trellis panel.  Optionally, adjacent panels can be grouped in a
common border.
}
\usage{
likertWeighted(x, data, xlim=c(-100,100),
                   x.at=seq(-100,100,25), x.labels=abs(x.at),
                   h.resizePanels=data$rowheights,
                   rightAxisLabels=format(round(h.resizePanels, digits),
                                          big.mark=","),
                   ylab=NULL,
                   axis.key.padding=10,
                   y.tck=c(0,3),
                   layout=c(1, nrow(data)),
                   as.percent=TRUE,
                   line.color="black",
                   box.ratio=1000,
                   digits=-3,
                   scales.cex=1,
                   between=list(y=0),
                   layer=TRUE,
                   ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a formula of the form \code{ ~ a + b + c | panelnames} where
\code{a}, code{b}, \code{c} are columns in \code{data} and \code{panelnames} is a column of
\code{data}
that will normally be a copy of \code{rownames(data)}.  The values in
\code{panelnames} must be unique.
The formula \code{ ~ . | panelnames} where \code{.} stands for the '\code{+}' of all
variables in the data.frame other than \code{panel.names} may be used.

This formula will be
passed to \code{\link[HH]{likert}}
}
  \item{data}{
    \code{data.frame}
  }
  \item{x.at, x.labels}{location of x tick-marks and value of
    corresponding labels.
    Same meaning as \code{scales=list(x=list(at=seq(-100,100,25),
                                             labels=abs(seq(-100,100,25))))}}
  \item{between}{
Standard argument to lattice functions, see
\code{\link[lattice]{xyplot}}.
In addition, adjacent panels with zero-valued y.between between them
will be grouped within the same borders, and adjacent panels with
non-zero-valued y.between between them will be be in separately bordered
groups.
See example EEE. 
}
  \item{xlim, ylab, layout, box.ratio}{
Standard arguments to lattice functions, see \code{\link[lattice]{xyplot}}.
}
  \item{digits}{
See \code{\link{round}}
}
  \item{axis.key.padding}{
See \code{\link[lattice]{trellis.par.get}}
}
  \item{as.percent, rightAxisLabels, h.resizePanels}{
See \code{\link[HH]{likert}}
}
\item{scales.cex}{Abbreviation for \code{scales=list(cex=scales.cex)}}
\item{y.tck}{Abbreviation for \code{scales=list(y=list(tck=y.tck)}}
\item{line.color}{Color to be used to surround sub-panels when
  \code{between=list(y=v)} has a non-zero vector for v.
}
  \item{layer}{
Logical, default \code{TRUE}.  If \code{TRUE} then put a border around
the sets of grouped panels.  If \code{FALSE}, then don't put a border.
}
  \item{\dots}{
additional arguments that will be passed through to
\code{\link[HH]{likert}}
and from there to lattice.
}
}
\details{
  In the standard \code{likert()} plot, all stacked bars have the same
  thickness.  The \code{likertWeighted} function weights the
  thickness of the bars by \code{h.resizePanels}.  When
  used with \code{as.percent=TRUE}, the i^{th} stacked bar is 100\% wide
  and \code{h.resizePanels}[i] units high.  As a consequence one square
  inch of plotting surface represents the same number of objects in all
  bars and sub-bars.
  Compare AAA, BBB, and CCC in the examples to see the effect.
}
\value{
 A \code{"trellis"} object containing the plot.  The plot will be
 automatically displayed unless the result is assigned to an object.
}
\author{Richard M. Heiberger, with contributions from Naomi B. Robbins <naomi@nbr-graphs.com>.

Maintainer: Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{likert}}
}
\examples{

tmp <- data.frame(matrix(1:12, 3, 4,
                           dimnames=list(c("A","B","C"),
                                         c(letters[4:7]))))
tmpcc <- cbind(tmp, rowheights=rowSums(tmp))

AAA <- likert(~ . , data=tmp, rightAxisLabels=tmpcc$rowheights,
              main="The total width of each bar is the sum of its magnitudes.
The width of each sub-bar is its magnitude.
All bars have the same thickness.")
AAA

BBB <- likert(~ . , data=tmp, as.percent=TRUE, rightAxisLabels=tmpcc$rowheights,
              main="The total width of each bar is 100%.
The width of each sub-bar is its proportion of its row.
All bars have the same thickness.")
BBB

CCC <- likertWeighted(~ . | rowheights, tmpcc,
                          as.percent=TRUE, xlim=c(-100,100),
                          rightAxisLabels=tmpcc$rowheights,
                          main="The total width of each bar is 100%.
The width of each sub-bar is its proportion of its row.
The thickness of each bar is the unique sum of its magnitudes.")
CCC



tmpdd <- cbind(tmp, rownames=row.names(tmp))

likertWeighted(~ . | rownames, tmpdd,
                   as.percent=TRUE, xlim=c(-100,100),
                   rightAxisLabels=tmpcc$rowheights,
                   main="Equal row thicknesses")

likertWeighted(~ . | rownames, tmpdd,
                   as.percent=TRUE, xlim=c(-100,100),
                   rightAxisLabels=tmpcc$rowheights,
                   h.resizePanels=rowSums(tmp),
                   main="Variable row thicknesses")

likertWeighted(~ . | rownames, tmpdd,
                   as.percent=TRUE, xlim=c(-100,100),
                   rightAxisLabels=c(1,3,3),
                   h.resizePanels=c(1,3,3),
                   ylab.right="Thicknesses Explicitly Specified",
                   main="Variable row thicknesses, with some duplication")

DDD <-
likertWeighted(~ . | rownames, tmpdd,
                   as.percent=TRUE, xlim=c(-100,100),
                   rightAxisLabels=paste(tmpcc$rowheights, c(1,3,3), sep=", "),
                   ylab.right="RowCount Total\nThicknesses Explicitly Specified",
                   h.resizePanels=c(1,3,3),
                   main="The total width of each bar is 100%.
The width of each sub-bar is its proportion of its row.
The thickness of each bar is the value of h.resizePanels.")
DDD

## comparisons
AAA
BBB
CCC
DDD

EEE <-
likertWeighted(~ . | rownames, tmpdd,
                   as.percent=TRUE, xlim=c(-100,100),
                   h.resizePanels=rowSums(tmp),
                   rightAxisLabels=rowSums(tmp),
                   between=list(y=c(0,1)),
                   ylab=c("C in its own group","A and B together"),
                   main="Illustrating between: between=list(y=c(0,1))
Adjacent A and B with y.between = 0 are in the same bordered group.
Adjacent B and C with y.between != 0 are in different bordered groups.")
EEE

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
