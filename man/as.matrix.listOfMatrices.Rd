\name{as.matrix.listOfNamedMatrices}
\alias{as.matrix.listOfNamedMatrices}
\alias{is.listOfNamedMatrices}
\alias{as.data.frame.listOfNamedMatrices}
\alias{as.listOfNamedMatrices}
\alias{print.listOfNamedMatrices}
\alias{as.MatrixList}
\alias{as.MatrixList.array}
\alias{print.MatrixList}
\alias{matrix}

\alias{[.listOfNamedMatrices}
\alias{as.listOfNamedMatrices.array}
\alias{as.listOfNamedMatrices.list}
\alias{as.listOfNamedMatrices.MatrixList}

\alias{as.likertDataFrame}
\alias{as.likertDataFrame.listOfNamedMatrices}
\alias{as.likertDataFrame.array}

\title{Convert a list of matrices to a single matrix}
\description{

  Convert a list of matrices to a single matrix.  This function is used
  to improve legibility of the printed object.  The
  \code{as.matrix.listOfNamedMatrices} display is easier to read when the rownames
  are very long, as in the example illustrated here.  Because the
  default print of the matrix repeats the rownames several times, with
  only a few columns of the data shown in each repetition, the actual
  matrix structure of the data values is obscured.

}
\usage{
\method{as.matrix}{listOfNamedMatrices}(x, abbreviate = TRUE, minlength = 4, ...)
is.listOfNamedMatrices(x,  xName=deparse(substitute(x)))
\method{as.data.frame}{listOfNamedMatrices}(x, ...)
as.listOfNamedMatrices(x,  xName=deparse(substitute(x)), ...)
\method{[}{listOfNamedMatrices}(x, ...)
\method{as.listOfNamedMatrices}{array}(x, xName=deparse(substitute(x)), ...)
\method{as.listOfNamedMatrices}{list}(x, xName=deparse(substitute(x)), ...)
\method{as.listOfNamedMatrices}{MatrixList}(x, xName=deparse(substitute(x)), ...)
\method{print}{listOfNamedMatrices}(x, ...)

as.MatrixList(x)
\method{as.MatrixList}{array}(x)
\method{print}{MatrixList}(x, ...)

as.likertDataFrame(x, xName=deparse(substitute(x)))
\method{as.likertDataFrame}{listOfNamedMatrices}(x, xName=deparse(substitute(x)))
\method{as.likertDataFrame}{array}(x, xName=deparse(substitute(x)))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Named list of matrices.  All matrices in the list should have the
    same number of columns and the same column names.  The list item
    names will normally be long.  The row names will normally be long.
    The number of rows and their names will normally differ across the
    matrices.   Each named item in the list may be a vector, matrix, array,
    data.frame, two-dimensional table, two-dimensional ftable, or
    two-dimensional structable
    For the \code{as.MatrixList} methods, an array.
}
  \item{\dots}{Other arguments.  Not used.}
  \item{abbreviate}{
    Logical.  If \code{TRUE}, then use the
    \code{\link[base]{abbreviate}}
    function on the item names and row names.
}
  \item{minlength}{the minimum length of the abbreviations.}
  \item{xName}{Name of the argument in its original environment.}
}
\value{
  The result of \code{as.listOfNamedMatrices} is a list with
  \code{class=c("listOfNamedMatrices", "list")}.

  The result of \code{as.matrix.listOfNamedMatrices} is an \code{rbind} of
  the individual matrices in the argument list \code{x}.  The rownames of
  the result matrix are constructed by pasting the abbreviation of the
  list item names with the abbreviation of the individual matrix rownames.
  The original names are retained as the \code{"Subtables.Rows"}
  attribute.

  The result of \code{is.listOfNamedMatrices} is logical value.

  \code{print.listOfNamedMatrices} prints
  \code{as.matrix.listOfNamedMatrices} of its argument and returns the
  original argument.

  \code{as.data.frame.listOfNamedMatrices(x, ...)} is an unfortunate
  kluge.  The result is the original \code{x} that has NOT been
  transformed to a \code{data.frame}.  A \code{warning} message is
  generated that states that the conversion has not taken place.  This
  kluge is needed to use \code{"listOfNamedMatrices"} objects with the
  \code{\link[Rcmdr]{Commander}} package because \code{Rcmdr} follows
  its calls to the R \code{\link[utils]{data}} function with an attempt,
  futile in this case, to force the resulting object to be a \code{data.frame}.

The \code{as.MatrixList} methods construct a list of matrices from an
array.  Each matrix has the first two dimensions of the array.  The
result list is itself an array defined by all but the first two
dimensions of the argument array.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{\code{\link{likert}}}
\examples{
data(ProfChal)

tmp <- data.matrix(ProfChal[,1:5])
rownames(tmp) <- ProfChal$Question
ProfChal.list <- split.data.frame(tmp, ProfChal$Subtable)

## Original list of matrices is difficult to read because
## it is displayed on too many lines.
ProfChal.list[2:3]

## Single matrix with long list item names and long row names
## of argument list retained as an attribute.
as.listOfNamedMatrices(ProfChal.list[2:3], minlength=6)

tmp3 <- array(1:24, dim=c(2,3,4), dimnames=list(A=letters[1:2], B=LETTERS[3:5], C=letters[6:9]))
tmp3
as.MatrixList(tmp3)

\dontrun{
sapply(as.MatrixList(tmp3), as.likert, simplify=FALSE)  ## odd number of levels.

data(NZScienceTeaching)
likert(Question ~ ., NZScienceTeaching)
likert(Question ~ . | Subtable, data=NZScienceTeaching)
likert(Question ~ . | Subtable, data=NZScienceTeaching,
       layout=c(1,2), scales=list(y=list(relation="free")))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
