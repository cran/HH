\name{partial.corr}
\alias{partial.corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{partial correlations}
\description{
  The partial correlation of x and y conditioning on z is the
  ordinary correlation of the residuals from the regression of x on z
  and the regression of y on z.
}
\usage{
partial.corr(vars, cond)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{matrix of data.frame of all the variables to be correlated.}
  \item{cond}{matrix of data.frame of all the variables to be
    conditioned on.}
}
\value{
matrix of partial correlations of the numeric variables in the argument
\code{vars} conditioning on the numeric variables in \code{cond}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
if.R(r=
partial.corr(longley[,1:3], longley[,4:6])
,s=
partial.corr(longley.x[,1:3], longley.x[,4:6])
)
}
\keyword{hplot}
