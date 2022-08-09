\name{InsertVerticalPanels}
\alias{InsertVerticalPanels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expand a 3D array on the second dimension, inserting empty layers where
the input vector has a \code{0} value.  A 2D argument \code{x} with \code{dim(x)==c(r,c)} is
first extended to 3D with \code{dim(x)==c(1,r,c)}, and then the result is
collapsed back to 2D.
}
\description{
Expand a 3D array on the second dimension, inserting empty layers where
the input vector has a \code{0} value.  A 2D argument \code{x} with \code{dim(x)==c(r,c)} is
first extended to 3D with \code{dim(x)==c(1,r,c)}, and then the result is
collapsed back to 2D.
}
\usage{
InsertVerticalPanels(x, expansion, newRowheights=5, newValue=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Three-dimensional array, for example, one defined as a set of matrices
for the \code{likert} and related functions.
\code{x[1,,]} and more generally \code{x[i,,]} will be an argument to \code{likert}.
}
  \item{expansion}{
Vector of \code{0} and \code{1}, with \code{1} indicating an existing layer in dimension 2, and \code{0} a
placeholder for where a new layer in dimension 2 should be inserted.
}
  \item{newRowheights}{
Value to be used for inserted row by \code{likertWeighted} function.
}
  \item{newValue}{
    Value to be inserted in all positions of inserted layer.
}
}
\value{
  Array similarly structured to the input array \code{x}, but with more
  layers on the second dimension.  The \code{"rowheights"} attribute
  gives the \code{rowheights} used by \code{EmphasizeVerticalPanels}.  The
  \code{newRows}
  gives the row (second dimension) numbers in the result that are the
  generated values.  All data items in the \code{newRows} will have
  value in the \code{newValue} argument.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{EmphasizeVerticalPanels}
\examples{

x <- array(1:24, c(3, 4, 2),
                dimnames = list(letters[1:3], LETTERS[4:7], letters[8:9]))
x

expansion <- c(1, 1, 0, 1, 0, 1)
result <- InsertVerticalPanels(x, expansion)
result

Pop.labels1 <- result[1,, ]
Pop.labels1[ attr(result, "newRows"),] <- " "

Pop.labels2 <- result[2,, ]
Pop.labels2[ attr(result, "newRows"),] <- " "

Pct.labels1 <- format(round(HH::rowPcts(result[1,, ])))
Pct.labels1[ attr(result, "newRows"),] <- " "

Pct.labels2 <- format(round(HH::rowPcts(result[2,, ])))
Pct.labels2[ attr(result, "newRows"),] <- " "

}
