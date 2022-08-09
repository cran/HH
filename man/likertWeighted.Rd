\name{likertWeighted}
\alias{likertWeighted}
\alias{likertWeighted.formula}
\alias{likertWeighted.array}
\alias{likertWeighted.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Special case wrapper for likert() when multiple columns are to have the
same bar thicknesses.  Uses formula with one or two conditioning variables.
}
\description{
Special case wrapper for likert() when multiple columns are to have the
same bar thicknesses.  Uses formula with one or two conditioning variables.
}
\usage{
likertWeighted(x, ...) ## generic

\method{likertWeighted}{array}(x, ..., C = 1, Q = 3, R = 2) ## array

\method{likertWeighted}{default}(x, ...) ## matrix, table, data.frame

\method{likertWeighted}{formula}(x, data,
               xlim=c(-100, 100),
               scales=list(y=list(relation="free", cex=1.3),
                           x=list(at=seq(-100, 100, 50),
                           labels=abs(seq(-100, 100, 50)), cex=.5)),
               box.ratio=1000,
               as.percent=TRUE, rightAxis=FALSE,
               between=list(x=1, y=0),
               strip=FALSE, strip.left=FALSE,
               par.settings=list(clip=list(panel="off")),
               h.resizePanels=1,
               auto.key.title=NULL,
               auto.key.columns=dim(data)[[2]] -
               NumberOfConditioningVariables(formula), ## excludes conditioning variables
               auto.key.cex=1.2,
               auto.key.cex.title=1.2,
               auto.key.lines.title=3,
               ylab=NULL,
               axis.top=dimnames(result)[[1]], ## Questions
               axis.top.row=1,
               ...)
               }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    For the default method, a matrix or data.frame or two-dimensional
    table.  For the array method, a two- or-three-dimensional array.
    For the formula method, a formula.
       }
  \item{formula}{
Standard trellis formula, usually \code{ ~ . | row + column} or
\code{ ~ . | row}
}
\item{data}{
A \code{data.frame} that has been constructed from a 2D object (\code{matrix}
or \code{table} or \code{data.frame}) to include an additional column \code{row}, or
constructed from a 3D \code{array} by \code{\link{toCQxR}} to include
two additional columns \code{group} and \code{row}.  The \code{default}
and \code{array} methods do that construction.
}
  \item{C, R, Q}{
Integers, one each of 1,2,3; positions of the three dimensions.  Used in
\code{array} method.  See \code{\link{toCQxR}}.
}
\item{xlim, between, strip, strip.left, par.settings, ylab}{
See \code{\link{xyplot}}
}
  \item{scales}{
    See \code{\link{xyplot}}.  For \code{likertWeighted}, when scales for x is
      changed, scales for y must be stated also.
}
  \item{box.ratio}{
See \code{\link{panel.bwplot}}.
}
  \item{as.percent, rightAxis, \dots, h.resizePanels}{
First see the formula method for \code{likertWeighted}, and then \code{\link{likert}}.
}
\item{auto.key.title, auto.key.columns, auto.key.cex,
    auto.key.cex.title, auto.key.lines.title}{
    Values which will be used in \code{trellis} argument
    \code{auto.key=list(title=auto.key.title, columns=auto.key.columns,
    cex=auto.key.cex, cex.title=auto.key.cex.title,
    lines.title=auto.key.lines.title)}
}
  \item{axis.top}{
Label to be placed at \code{x=0} for top (and other specified) panel of each column.
}
  \item{axis.top.row}{
Which rows will have \code{axis.top} displayed.
}
}
\value{
A \code{\link{likert}} plot as a \code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link{likert}}
}
\examples{

## simplest 2D example
tmp <- matrix(1:12, 3, 4,
              dimnames=list(c("A","B","C"),
                            c(letters[4:7]))) * c(1,2,3)
tmp
rowSums(tmp)

likertWeighted(tmp,
               h.resizePanels=rowSums(tmp),
               main="likertWeighted, simplest example,
                     defaults to Percent,
                     specified row thicknesses")

## Same example with explicit use of the formula method
## (default method does this for you).
tmpdd <- data.frame(tmp, row=row.names(tmp))
tmpdd
likertWeighted(~ . | row, tmpdd,            ## tmpdd
               h.resizePanels=rowSums(tmp), ## tmp
main="likertWeighted,
same example but with explicit formula method")

## show subgroups
likertWeighted(tmp,
               h.resizePanels=rowSums(tmp),
               between=list(y=c(0, 1)),
               ylab=c("C in its own group","A and B together"),
               main="between=list(y=c(0,1) ## standard lattice between argument
Adjacent A and B with y.between = 0 are in the same bordered group.
Adjacent B and C with y.between != 0 are in different bordered groups.")


## simplest 3D example
## This is natural when multiple questions are asked of the
## same set of respondents in a survey.
## This example simulates that situation.
##
tmp3D <- abind::abind(h=tmp, i=tmp, j=tmp, along=3)
tmp3D[1,,"i"] <- tmp3D[1,c(4,2,1,3),"h"]
tmp3D[2,,"i"] <- tmp3D[2,c(2,4,3,1),"h"]
tmp3D[3,,"i"] <- tmp3D[3,c(4,1,2,3),"h"]
tmp3D[1,,"j"] <- tmp3D[1,c(4,3,2,1),"h"]
tmp3D[2,,"j"] <- tmp3D[2,c(1,4,3,2),"h"]
tmp3D[3,,"j"] <- tmp3D[3,c(2,4,3,1),"h"]
## now
rowSums(tmp3D[,,1]) == rowSums(tmp3D[,,2])
rowSums(tmp3D[,,1]) == rowSums(tmp3D[,,3])


likertWeighted(tmp3D, h.resizePanels=rowSums(tmp3D[,,1]),
               main="simplest 3D example, array method")

likertWeighted(tmp3D, h.resizePanels=rowSums(tmp3D[,,1]),
               between=list(x=1, y=c(0, 1)),
               main="simplest 3D example, array method, with subgroups")

## Same example with explicit use of the formula method
## (array method does this for you).
tmp3Ddf <- toCQxR(tmp3D)
dimnames(tmp3Ddf)
tmp3Ddf
likertWeighted(~ . | group + row, tmp3Ddf, h.resizePanels=rowSums(tmp3D[,,1]),
               main="simplest 3D example, formula method")


}
