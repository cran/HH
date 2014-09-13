\name{pdiscunif}
\alias{pdiscunif}
\alias{qdiscunif}
\alias{ddiscunif}
\alias{rdiscunif}

\title{Discrete Uniform Distribution}
\description{Discrete Uniform Distribution}
\usage{
pdiscunif(q, size)
qdiscunif(p, size)
ddiscunif(q, size)
rdiscunif(n, size)
}
\arguments{
\item{size}{parameter of distribution.  Numbers from 1 to \code{} size
  are equally likely.}
\item{q}{Quantiles.}
\item{p}{Probability.}
\item{n}{number of items in the random sample.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
  q <- seq(-.5, 7.5, .5)

  pp <- pdiscunif(q, 6)

  ## xyplot(pp ~ q,
  ##        scales=list(
  ##        x=list(at=floor(min(q)):ceiling(max(q))),
  ##        y=list(at=seq(0, 1, .1))))

  qq <- qdiscunif(pp, 6)

  dd <- ddiscunif(q, 6)

  cbind(q, pp, qq, dd)

  rdiscunif(12, 6)
}
\keyword{distribution}
