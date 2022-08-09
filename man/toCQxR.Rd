\name{toCQxR}
\alias{toCQxR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape a 3-way array to a 2-way data.frame that can can be used with a
trellis conditioning formula to get the three-way behavior.
Used with likertWeighted().
}
\description{
Reshape a 3-way array to a 2-way data.frame that can can be used with a
trellis conditioning formula to get the three-way behavior.
Used with \code{likertWeighted()}.
}
\usage{
toCQxR(x, C = 1, R = 2, Q = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Three-way array, with dimensions "Classification", "Responses",
"Questions" in some order.
}
  \item{C, R, Q}{
Integers, one each of 1,2,3; positions of the three dimensions.
}
}
\value{
Data.frame with CQ rows and Q + N columns, where N is either 1 or 2 for
the number of condition variables in the formula for \code{\link{likertWeighted}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link{likertWeighted}}
}
\examples{
tmp3 <- array(1:40, c(4,5,2), list(LETTERS[1:4], LETTERS[5:9], LETTERS[10:11]))
tmp3

toCQxR(tmp3)
}
