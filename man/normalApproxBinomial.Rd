\name{normalApproxBinomial}
\alias{normalApproxBinomial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots to illustrate Normal Approximation to the
Binomial---hypothesis tests or confidence intervals.
}
\description{
Plots to illustrate Normal Approximation to the
Binomial---hypothesis tests or confidence intervals.
}
\usage{
normalApproxBinomial(p0= if (number.vars==1) .5 else 0,
                     p1=NA, p2=NA,
                     p.hat=if (number.vars==1) .75 else 0,
                     n=1,
                     xlim=if (number.vars==1) c(0,1) else c(-1,1),
                     ylim=c(0, 5),
                     type=c("hypothesis","confidence"),
                     alpha.left=if (type=="hypothesis") 0 else .025,
                     alpha.right=if (type=="hypothesis") .05 else .025,
                     xlab=if (number.vars==1)
                            "w = p = population proportion"
                          else
                            "w = p[1] - p[2] :: population proportions", ...,
                     number.vars=if (!is.na(p1) && !is.na(p2)) 2 else 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{
Null hypothesis value of \eqn{p}.
}
  \item{p1}{
    Alternate hypothesis value of \eqn{p} for one-sample cases.
    Second sample value of \eqn{p} for two-sample cases.
}
  \item{p2}{
Second sample value of \eqn{p}.
}
  \item{p.hat}{
Observed  value of \eqn{p}.
}
  \item{n}{
Number of observations (for example, number of coins tossed).
}
  \item{xlim, ylim, xlab}{
Standard \code{\link[lattice]{xyplot}} arguments...
}
  \item{type}{
    "hypothesis" for a Hypothesis Test graph, or "confidence" for a
    Confidence Interval graph.
}
  \item{\dots, alpha.left, alpha.right}{
Additional arguments forwarded to \code{\link{NTplot}}.
}
\item{number.vars}{
  Number of variables. 1 for a one-sample test, 2 for two-sample tests
  and paired tests.
}
}
\details{
This is a wrapper function for the plots in \code{\link{NTplot}}.
}
\value{
\code{"trellis"} object.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\examples{
NTplot(distribution.name="binomial", n=20, ylim=c(0,4.2), p1=.8)
NTplot(distribution.name="binomial", n=20, type="confidence", ylim=c(0,4.2))
\dontrun{
NTplot(distribution.name="binomial", n=20, zaxis=TRUE, z1axis=TRUE,
       p1=.8678, ylim=c(0, 5.2))
NTplot(p0=.4, p.hat=.65, p1=.7, distribution.name="binomial", n=15)
NTplot(p.hat=.65, distribution.name="binomial", n=15, type="confidence")
}
\dontrun{ ## these are interactive and won't work in R CMD check
NTplot(distribution.name="binomial", n=20, ylim=c(0,4.2), p1=.8, shiny=TRUE)
NTplot(p0=.4, p.hat=.65, p1=.7, distribution.name="binomial", n=15, shiny=TRUE)
NTplot(p.hat=.65, distribution.name="binomial", n=15, type="confidence", shiny=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
