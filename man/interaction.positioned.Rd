\name{interaction.positioned}
\alias{interaction.positioned}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{interaction method for positioned factors.}
\description{
  This is intended to be a method for \code{interaction} for positioned
  factors.  Since \code{interaction} is not currently implemented as
  a generic, \code{interaction.positioned} is a standalone function.
  The result is assigned a \code{position}.  The position for each interaction
  level is the position of the corresponding \code{a} factor plus a
  scaled level of the \code{b} factor.  The default scale is .1.  
}
\usage{
interaction.positioned(...,  ## exactly two factors
             drop = FALSE, sep = ".",
             b.offset=0,
             b.scale=.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{exactly two factors.  The first
    factor \code{a} is used as the major factor in sort order.
    The second factor \code{b} is used as minor factor in sort order.}
  \item{b.offset}{amount added to \code{position(b)} to adjust appearance.}
  \item{b.scale}{scale to relate units of \code{position(a)} to
    units of \code{position(b)}.}
  \item{drop, sep}{See \code{\link{factor}}.}
}
\value{
\code{"positioned"} object containing the ordinary interaction with a
\code{"position"} attribute.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{positioned}}.}
\examples{
a <- positioned(letters[c(1,2,3,1,2,3)], value=c(1,4,9))
b <- positioned(LETTERS[c(4,4,4,5,5,5)], value=c(1,2))
a.b <- interaction.positioned(a, b)
a.b.2 <- interaction.positioned(a, b, b.scale=.2)
b.a <- interaction.positioned(b, a)
}
\keyword{ dplot }
\keyword{ regression }
