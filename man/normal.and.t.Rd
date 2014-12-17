\name{NormalAndTplot}
\alias{NormalAndTplot}
\alias{NormalAndT}
\alias{NormalAndTplot.default}
\alias{NormalAndTplot.htest}
\title{Specify plots to illustrate Normal and t Hypothesis Tests or Confidence Intervals.}
\description{Specify plots to illustrate Normal and t Hypothesis Tests
  or Confidence Intervals.}
\usage{
NormalAndTplot(mean0, ...)
\method{NormalAndTplot}{default}(mean0=0,
             mean1=NA,
             xbar=NA,
             sd=1, df=Inf, n=1,
             xlim=c(-3, 3), ylim, alpha.right=.05, alpha.left=0,
             float=TRUE, ntcolors="original",
             digits=4, digits.axis=digits, digits.float=digits,
             distribution.name=c("normal","z","t"),
             type=c("hypothesis", "confidence"),
             zaxis=FALSE, z1axis=FALSE,
             cex.z=.5, cex.prob=.6, cex.top.axis=1,
             main, xlab, ylab,
             prob.labels=(type=="hypothesis"),
             xhalf.multiplier=1,
             cex.main=1,
             key.axis.padding=4.5, ...)
\method{NormalAndTplot}{htest}(mean0, type="hypothesis", xlim=NULL, ...,
             xbar, sd, df, n, alpha.left, alpha.right, distribution.name, sub
             ## these input arguments sfter ... will be ignored
             )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean0}{
    Null hypothesis \eqn{mu_0}.
    When graphing a confidence interval, \code{mean0} will be used for
    \code{xbar} should \code{xbar} itself have the value \code{NA}.
}
  \item{mean1}{
Alternative hypothesis \eqn{mu_1}.
}
  \item{xbar}{
Observed \eqn{\bar(x)}.
}
  \item{sd}{
Standard deviation \eqn{\sigma} for normal-, or \eqn{s} for \eqn{t}-distribution.
}
  \item{df}{
Degrees of freedom for \eqn{t}-distribution.
}
\item{n}{
  Number of observations.
}
\item{main, xlab, ylab, xlim, ylim, sub}{
  Standard \code{\link[lattice]{xyplot}} arguments.  Default values
  are constructed if these arguments are missing.
}
\item{\dots}{
  Additional \code{\link[lattice]{xyplot}} arguments.
}
  \item{alpha.left, alpha.right}{For \code{type="hypothesis"},
the sum of these two numbers is the probability of the Type I Error
\eqn{\alpha}.  When both of these numbers are positive, there is a two-sided
test.  Note that it is not required that they be equal.  If one of the
numbers is 0, then it is a one-sided test.  For
\code{type="confidence"}, 1 minus the sum of these two numbers is the
confidence level.
}
  \item{float}{
Logical.  If \code{TRUE}, then the probabilities \eqn{\alpha}, \eqn{\beta}, power, and
\eqn{p}-values or the confidence value are displayed on the graph.  If \code{FALSE}, these values
are not displayed.
}
  \item{ntcolors}{
Vector of colors used in the graph.  The default value is
\code{"original"}, which implies the ten colors \code{c(col.alpha = "blue", col.notalpha = "lightblue",
    col.beta = "red", col.power = "pink", col.pvalue = "green",
    col.pvaluetranslucent = green127, col.critical = "gray50",
    col.border = black127, col.text = "black", col.conf = "lightgreen")}.
  An alternative is \code{"stoplight"} which implies the ten colors
  \code{c(col.alpha = "red", col.notalpha = "honeydew2",
    col.beta = "orange", col.power = "pink", col.pvalue = "blue",
    col.pvaluetranslucent = blue127, col.critical = "gray50", col.border
    = black127, col.text = "black", col.conf = "lightgreen")}.
     The partially transparent colors are: black127="#0000007F",
    green127="#00FF007F", blue127="#0000FF7F".
 The user can enter any color scheme by specifying a vector of ten
named colors.  The names are:
\code{col.alpha, col.notalpha, col.beta, col.power, col.pvalue,
col.pvaluetranslucent, col.critical, col.border, col.text, col.conf}.
}
  \item{digits.axis, digits.float, digits}{
    \code{digits.axis} is the number of significant digits for the top
    axis.  \code{digits.float} is the number of significant digits for
    the floating probability values on the graph.  \code{digits} is a
    convenience argument to set both \code{digits.axis} and
    \code{digits.float} at the same time.  These number is passed to the
    \code{\link[base]{format}} function.
  }
  \item{distribution.name}{
    Name of distribution.
  }
  \item{type}{
    "hypothesis" for a Hypothesis Test graph, or "confidence" for a
    Confidence Interval graph.
  }
  \item{z1axis, zaxis}{
    Logical.  Should the \eqn{z_1}-axis centered on \eqn{mu_1} or the
    \eqn{z_0}-axis centered on \eqn{mu_0} be displayed?
  }
  \item{cex.z, cex.prob, cex.top.axis, cex.main}{
    \code{cex.z} is the \code{cex} value for the \eqn{z} and \eqn{z_1}
    axes on the plot.  \code{cex.prob} is the \code{cex} value for the
    floating probabilities on the graph. \code{cex.top.axis} is the \code{cex} value
    for the top axis values.  \code{cex.main} is the \code{cex} value for
    the main title.
  }
\item{key.axis.padding}{tuning constant to create additional room above the
  graph for a larger \code{cex.main} to fit.}
  \item{prob.labels}{logical. If \code{TRUE} label the floating
    probability values with their name, such as \eqn{alpha}.  If
    \code{FALSE},
    then don't label them.  The default is \code{TRUE} for
    \code{type="hypothesis"}
    and \code{FALSE} for \code{type="confidence"}.
  }
  \item{xhalf.multiplier}{
    Numerical tuning constant to control the width of the floating
    probability values.  Empirically, we need a smaller value for the
    \pkg{shiny}
    app then we need for direct writing onto a graphic device.
  }
}
\details{
This graphs produced by this single function cover most of the first semester
introductory Statistics course.
}
\value{
\code{"trellis"} object.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\note{
  This function is built on \pkg{lattice} and \pkg{latticeExtra}.
  It supersedes the similar function
\code{normal.and.t.dist} built on base graphics that is used in many
  displays in the book by Erich Neuwirth and me: \emph{R through Excel}, Springer
(2009).
\url{http://www.springer.com/978-1-4419-0051-7}.  Many details,
  particularly the
  alternate color scheme and the concept of floating probability labels,
  grew out of discussions that Erich and I have had since the book was published.
This version incorporates some ideas suggested by Moritz Heene.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\examples{
   NormalAndTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5))
   NormalAndTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5), distribution.name="t", df=4)
   NormalAndTplot(mean0=100, sd=12, mean1=113,  xbar=105,  xlim=c(92, 120), n=20)
   NormalAndTplot(mean0=100, sd=12, mean1=113,  xbar=105,  xlim=c(92, 120), n=20, z1axis=TRUE)
   NormalAndTplot(mean0=100, sd=12,  xbar=105,  xlim=c(92, 108), n=20, ntcolors="stoplight")
   NormalAndTplot(xbar=95, sd=10, xlim=c(65, 125), type="confidence",
                  alpha.left=.025, alpha.right=.025)

\dontrun{
     shiny::runApp(file.path(system.file(package="HH"), "shiny"))
}

\dontrun{
## mean1 and xbar
  NormalAndTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5))
  NormalAndTplot(mean0=0, mean1=-2, xbar=-1.8, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0)
  NormalAndTplot(mean0=0, mean1=2,  xbar=2.1,  xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025)
  NormalAndTplot(mean0=0, mean1=-2, xbar=-2.1, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025)

## mean1
  NormalAndTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5))
  NormalAndTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0)
  NormalAndTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025)
  NormalAndTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025)

## xbar
  NormalAndTplot(mean0=0, mean1=NA, xbar=1.8,  xlim=c(-3, 5))
  NormalAndTplot(mean0=0, mean1=NA, xbar=-1.8, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0)
  NormalAndTplot(mean0=0, mean1=NA, xbar=2.1,  xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025)
  NormalAndTplot(mean0=0, mean1=NA, xbar=-2.1, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025)

## t distribution
## mean1 and xbar
  NormalAndTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5),
                 distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=-2, xbar=-1.8, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=2,  xbar=2.1,  xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=-2, xbar=-2.1, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## mean1
  NormalAndTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
                 distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## xbar
  NormalAndTplot(mean0=0, mean1=NA, xbar=1.8,  xlim=c(-3, 5),
                 distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=NA, xbar=-1.8, xlim=c(-5, 3),
                 alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=NA, xbar=2.1,  xlim=c(-3, 5),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NormalAndTplot(mean0=0, mean1=NA, xbar=-2.1, xlim=c(-5, 3),
                 alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## confidence intervals

  NormalAndTplot(mean0=0, xlim=c(-3, 4), type="confidence")
  NormalAndTplot(xbar=01, xlim=c(-3, 4), type="confidence")
  NormalAndTplot(mean0=0, xlim=c(-4, 3), type="confidence",
                 alpha.left=.05,  alpha.right=0)
  NormalAndTplot(mean0=0, xlim=c(-3, 3), type="confidence",
                 alpha.left=.025, alpha.right=.025)
  NormalAndTplot(mean0=95, sd=10, xlim=c(65, 125), type="confidence",
                 alpha.left=.025, alpha.right=.025)
  NormalAndTplot(mean0=95, sd=10, xlim=c(65, 125), type="confidence",
                 alpha.left=.025, alpha.right=.025,
                 distribution="t", df=10)

}
}
