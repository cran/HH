\name{logit}
\alias{logit}
\alias{antilogit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logistic function and its inverse.}
\description{
Logistic function and its inverse.
}
\usage{
logit(p)
antilogit(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Probability value, a vector of numbers between 0 and 1, inclusive.}
  \item{x}{Real number, a vector of numbers between \code{-Inf} and \code{Inf}.}
}
\value{
  Vector of real values \code{log(p/(1-p))} for \code{logit}.
  Vector of probabilities \code{exp(x)/(1+exp(x))} for \code{antilogit} with
  boundary values of \code{-Inf} and \code{Inf} for \code{x} correctly handled.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
logit(seq(0,1,.1))
antilogit(logit(seq(0,1,.1)))
}
\keyword{ math }
