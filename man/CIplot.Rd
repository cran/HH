\name{CIplot}
\alias{CIplot}
\alias{confintervaldata}
\alias{confinterval.matrix}
\alias{confintervalplot}
\alias{shiny.CIplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Illustration of the meaning of confidence levels.
}
\description{
Illustration of the meaning of confidence levels.  Generate sets of
confidence intervals for independent randomly generated sets of normally
distributed numbers.  Low confidence levels give narrow intervals that
are less likely to bracket the true value.  Higher confidence levels
increase the probability of bracketing the true value, and are also much
wider and therefore less precise.  The shiny app can animate how the
increase in confidence level and width leads to a consequent decrease in
precision.
}
\usage{
CIplot(n.intervals = 100,
       n.per.row = 40,
       pop.mean = 0,
       pop.sd = 1,
       conf.level = 0.95,
       ...)

confintervaldata(n.intervals = 100,
                 n.per.row = 40,
                 pop.mean = 0,
                 pop.sd = 1,
                 conf.level = 0.95,
                 seed,
                 ...)

confinterval.matrix(x,
                    conf.level = attr(x, "conf.level"),
                    ...)

confintervalplot(x.ci,
                 n.intervals = nrow(x.ci),
                 pop.mean = attr(x.ci, "pop.mean"),
                 pop.sd = attr(x.ci, "pop.sd"),
                 n.per.row = attr(x.ci, "n.per.row"),
                 xlim, ylim, ...)

shiny.CIplot(height = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.intervals}{
    Number of sets of observations to generate.
    Each set leads to one confidence interval on the plot.
}
  \item{n.per.row}{
Number of observations in each set.
}
  \item{pop.mean, pop.sd}{
Population mean and standard deviation for generated set of
\code{n.per.row} independent normally distributed random numbers.
}
  \item{conf.level}{
Confidence level of each of the \code{n.per.row} confidence intervals
calculated from the generated datasets.
}

\item{seed}{Standard argument to \code{\link[stats:Normal]{rnorm}}.}
\item{x}{Output matrix from \code{confintervaldata}.}
\item{x.ci}{Output \code{data.frame} from \code{confinterval.matrix}.}
\item{xlim, ylim}{Standard \code{\link[lattice]{xyplot}} arguments.}
\item{height}{Height of graph on web page in pixels.}


\item{\dots}{
Additional arguments.  For \code{CIplot},  \code{seed} will be forwarded
to \code{confintervaldata}, and \code{xlim} and \code{ylim} will be
forwarded to \code{confintervalplot}.  Any other additional arguments
will be ignored.
}
}
\details{
The shiny app has sliders for the \code{n.intervals}, \code{n.per.row},
\code{pop.mean}, \code{pop.sd}, and \code{conf.level}.
Changes in the \code{conf.level} slider, either manually by animation,
use the same set of generated data to show how increasing the confidence
level increases the width of the confidence interval and consequently
decreases the precision of the interval estimator.
}
\value{
  \code{CIplot} and \code{confintervalplot} return a \code{"trellis"}
  plot containing a plot of Confidence Intervals.

\code{confintervaldata} returns a matrix of \code{n.intervals} rows by
  \code{n.per.row} columns of independent normally distributed random
  numbers.
  The matrix has a set of attributes recording the arguments to the
  function.

  \code{confinterval.matrix} returns a \code{data.frame} of  \code{n.intervals}
  with three columns containing the lower bound, center, and upper bound
  of the confidence interval for each row of its input matrix.
  The \code{data.frame} has a set of attributes recording the arguments to the
  function.

  \code{shiny.CIplot} returns a shiny app object which, when printed,
  runs a shiny app displaying the Confidence Interval plot and several
  slider controls.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{

## A. from the console

## example 1
CIplot()

## example 2
\dontrun{
CIplot(n.intervals=100,
       n.per.row=40,
       pop.mean=0,
       pop.sd=1,
       conf.level=.95)
}

## example 3
\dontrun{
tmp.data <- confintervaldata()
tmp.ci <- confinterval.matrix(tmp.data)
confintervalplot(tmp.ci)
}

## example 4
\dontrun{
tmp.data <- confintervaldata(n.intervals=100,
                             n.per.row=40,
                             pop.mean=0,
                             pop.sd=1,
                             conf.level=.95)
tmp.ci <- confinterval.matrix(tmp.data)
confintervalplot(tmp.ci)
}


## B. shiny, initiated from the console

## example 5
\dontrun{
shiny.CIplot()
}

## example 6
\dontrun{
shiny.CIplot(height=800)  ## px
## take control of the height of the graph in the web page
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ dynamic }% __ONLY ONE__ keyword per line
\keyword{ confidence }% __ONLY ONE__ keyword per line
\keyword{shiny}
