\name{logit}
\alias{logit}
\alias{antilogit}
\alias{odds}
\alias{antiodds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logistic and odds functions and their inverses.}
\description{
Logistic and odds functions and their inverses.
}
\usage{
logit(p)
antilogit(x)

odds(p)
antiodds(o)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Probability value, a vector of numbers between 0 and 1, inclusive.}
  \item{x}{Real number, a vector of numbers between \code{-Inf} and \code{Inf}.}
  \item{o}{Real number, a vector of numbers between \code{0} and \code{Inf}.}
}
\value{
  Vector of real values \code{log(p/(1-p))} for \code{logit}.
  Vector of probabilities \code{exp(x)/(1+exp(x))} for \code{antilogit} with
  boundary values of \code{-Inf} and \code{Inf} for \code{x} correctly handled.
  Vector of real values \code{p/(1-p)} for \code{odds}.
  Vector of probabilities \code{o/(o+1)} for \code{antiodds} with the
  boundary value of \code{Inf} for \code{o} correctly handled.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
logit(seq(0, 1, .1))
antilogit(logit(seq(0, 1, .1)))

odds(seq(0, 1, .1))
antiodds(odds(seq(0, 1, .1)))
}
\keyword{ math }
