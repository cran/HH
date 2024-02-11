\name{z.test}
\alias{z.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Z test for known population standard deviation }
\description{
  Compute the test of hypothesis and compute confidence interval on the
  mean of a population when the standard deviation of the population is known.
}
\usage{
z.test(x, mu = 0, stdev, alternative = c("two.sided", "less", "greater"),
  sd = stdev, n=length(x), conf.level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Vector of data values or the mean of the data. }
  \item{mu}{ Hypothesized mean of the population. }
  \item{stdev}{ Known standard deviation of the population. }
  \item{alternative}{ Direction of the alternative hypothesis. }
  \item{sd}{ Alternative to \code{stdev} }
  \item{n}{ The sample size if \code{x} is the sample mean. }
  \item{conf.level}{ Confidence level for the interval computation. }
  \item{\dots}{ Additional arguments are silently ignored. }
}
\details{
  Many introductory statistical texts introduce inference by using the Z
  test and Z based confidence intervals based on knowing the population
  standard deviation.  Most statistical packages do not include
  functions to do Z tests since the T test is usually more appropriate
  for real world situations.  This function is meant to be used during
  that short period of learning when the student is learning about
  inference using Z procedures, but has not learned the T based
  procedures yet.  Once the student has learned about the T
  distribution the \code{t.test} function should be used instead of this
  one (but the syntax is very similar, so this function should be an
  appropriate introductory step to learning \code{t.test}).
}
\value{
  An object of class \code{htest} containing the results
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{538280@gmail.com} }
\note{ This function should be used for learning only, real data should
  generally use \code{t.test}.
These files \code{z.test.R} and \code{z.test.Rd} are from
the recently orphaned package TeachingDemos_2.12.1
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{t.test}}, \code{\link{print.htest}} }
\examples{
x <- rnorm(25, 100, 5)
z.test(x, 99, 5)

}
\keyword{ htest }% at least one, from doc/KEYWORDS

