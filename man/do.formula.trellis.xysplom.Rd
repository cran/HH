\name{do.formula.trellis.xysplom}
\alias{do.formula.trellis.xysplom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interprets model formulas for xysplom and extended bwplots}
\description{
Interprets a model formula in the context of its data.frame.
}
\usage{
do.formula.trellis.xysplom(formula, data, na.action = na.pass)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{model formula}
  \item{data}{data.frame}
  \item{na.action}{see \code{\link[stats]{na.action}}}
}
\value{
A list containing three data.frames and three formula, one for each.
  \item{x }{\code{data.frame} containing the variables on the right-hand side
    of the model formula.}
  \item{y }{\code{data.frame} containing the variables on the left-hand side
    of the model formula.}
  \item{g }{\code{data.frame} containing the variables, if any, after
    the conditioning bar \code{|} of the model formula.}
  \item{x.formula }{\code{formula} containing the right-hand side
    of the model formula.}
  \item{y.formula }{\code{formula} containing the left-hand side
    of the model formula.}
  \item{g.formula }{\code{formula} containing the formula after the
    conditioning bar \code{|} of the model formula.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[stats]{formula}}, \code{\link[stats]{na.action}}}
\examples{
tmp <- data.frame(y=1, x=2, z=3, g=4)
do.formula.trellis.xysplom( y ~ x + z | g, data=tmp)
}
\keyword{models }
