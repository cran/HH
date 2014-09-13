\name{interval}
\alias{interval}
\alias{interval.glm}

\title{Prediction and Confidence Intervals for glm Objects}
\description{Prediction and Confidence Intervals for \code{glm} Objects}
\usage{
interval(glm.object, ...)
\method{interval}{glm}(glm.object, linkfit.object,
         type = c("link", "response"),
         conf.level = 0.95, ...)
}

\arguments{
  \item{glm.object}{result from a call to the \code{glm} function.}
  \item{linkfit.object}{result from a call to the \code{predict}
  function for the \code{glm.object} with \code{type="link", se.fit=TRUE}.}
  \item{type}{Either \code{"link"} or \code{"response"}.  See
  \code{\link[stats]{predict.glm}} for details.}
  \item{conf.level}{Confidence level, for example \code{.95} for 95\%.}
  \item{\dots}{Other arguments to be passed to \code{predict.glm}.}
}

\value{Matrix with five columns: \code{fit, ci.low, ci.hi, pi.low,
    pi.hi} and as many rows as \code{predict.glm} returns.}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
data(spacshu)
spacshu.bin.glm <- glm(damage ~ tempF, data=spacshu, family=binomial)

## observed data
spacshu.interval          <- interval(spacshu.bin.glm)

## new data, link
spacshu.interval.link     <- interval(spacshu.bin.glm, newdata=data.frame(tempF=30:85))

## new data, response
spacshu.interval.response <- interval(spacshu.bin.glm, newdata=data.frame(tempF=30:85),
                                      type="response")
}
\keyword{regression}
