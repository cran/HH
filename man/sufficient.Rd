\name{sufficient}
\alias{sufficient}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates the mean, standard deviation, and number of
  observations in each group of a data.frame that has one continuous
  variable and two factors.
}
\description{
Calculates the mean, standard deviation, and number of
  observations in each group of a data.frame that has one continuous
  variable and two factors.
}
\usage{
sufficient(x,
           yname = dimnames(x)[[2]][[1]],
           factor.names.keep = dimnames(x)[[2]][-c(1, 2)])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{data.frame} containing a continuous variable and two factors.}
  \item{yname}{Character name of response variable.}
  \item{factor.names.keep}{Character vecotr containing the names of two
    factors in the \code{x} data.frame.}
}
\value{
  Data.frame containing five columns and as many rows as are implied by
  the crossing of the two factors.  Each row contains the mean in a
  column with the name \code{yname} and its factor values in columns
  named with the name in \code{factor.names.keep}.  The standard deviation of
  the observations in the group are in the column \code{"sd"} and the number
  of observations in the group is in the column \code{"nobs"}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{intxplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{univar}

