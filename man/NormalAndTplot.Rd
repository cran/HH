\name{NormalAndTplot}
\alias{NormalAndTplot}
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
             df=Inf, n=1,
             sd=1,
             xlim=c(-3, 3)*sd/sqrt(n) + range(c(mean0, mean1, xbar), na.rm=TRUE),
             ylim, alpha.right=.05, alpha.left=0,
             float=TRUE, ntcolors="original",
             digits=4, digits.axis=digits, digits.float=digits,
             distribution.name=c("normal","z","t","binomial"),
             type=c("hypothesis", "confidence"),
             zaxis=FALSE, z1axis=FALSE,
             cex.z=.5, cex.xbar=.5, cex.y=.5, cex.prob=.6, cex.top.axis=1,
             cex.left.axis=1, cex.pb.axis=1,
             cex.xlab=1, cex.ylab=1.5, cex.strip=1,
             main=NA, xlab, ylab,
             prob.labels=(type=="hypothesis"),
             xhalf.multiplier=1,
             yhalf.multiplier=1,
             cex.main=1,
             key.axis.padding=4.5,
             number.vars=1,
             sub=NULL,
             NTmethod="default",
             power=FALSE,
             beta=FALSE,
              ...)
\method{NormalAndTplot}{htest}(mean0, type="hypothesis", xlim=NULL, mean1=NA, ...,
             xbar, sd, df, n, alpha.left, alpha.right, ## ignored
             distribution.name, sub ## these input arguments will be ignored
             )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean0}{
    Null hypothesis \eqn{\mu_0}.
    When graphing a confidence interval, \code{mean0} will be used for
    \code{xbar} should \code{xbar} itself have the value \code{NA}.
    For the \code{htest} method, \code{mean0} is an \code{"htest"}
    object.
    See \code{\link{NTplot}} for more information.
}
  \item{mean1}{
Alternative hypothesis \eqn{\mu_1}.
}
  \item{xbar}{
Observed \eqn{\bar{x}}.
}
  \item{sd}{
Standard deviation in the data scale \eqn{\sigma} for normal-, or \eqn{s} for \eqn{t}-distribution.
}
%%   \item{stderr}{
%% Standard deviation in the \eqn{bar{x}} scale \eqn{sigma_{\bar{x}}} for
%% normal-, or \eqn{s_{\bar{x}}} for \eqn{t}-distribution.
%% }
  \item{df}{
Degrees of freedom for \eqn{t}-distribution.
}
\item{n}{
  Number of observations per group.
}
\item{main, xlab, ylab, xlim, ylim, sub}{
  Standard \code{\link[lattice]{xyplot}} arguments.  Default values
  are constructed if these arguments are missing. The input value
  \code{main=NA}
  forces a new constructed \code{main} instead of using the \code{main} coming in through the
  \code{htest} methods.
}
\item{\dots}{
  Additional \code{\link[lattice]{xyplot}} arguments.
}
\item{number.vars}{
  Number of variables. 1 for a one-sample test, 2 for two-sample tests
  and paired tests.
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
\code{"original"} and two named alternatives are \code{"stoplight"} and
\code{"BW"}.
The sets of colors associated with these three named sets are shown in a \code{dontrun}
section of the examples.
 The user can enter any other color scheme by specifying a vector of ten
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
  \item{zaxis, z1axis}{
    Logical or list.  Should the \eqn{z}-axis centered on \eqn{\mu_0}, or the
    \eqn{z_1}-axis centered on \eqn{\mu_1}, be displayed?  The list
    version of the argument must have two components \code{at} and \code{labels} as
    specified in \code{\link[lattice]{panel.axis}}.
  }
  \item{cex.z, cex.xbar, cex.y, cex.prob, cex.top.axis,
    cex.left.axis, cex.pb.axis, cex.xlab, cex.ylab, cex.strip,
    cex.main}{
    \code{cex.z} is the \code{cex} value for the \eqn{z} and \eqn{z_1}
    axes on the plot.  \code{cex.prob} is the \code{cex} value for the
    floating probabilities on the graph. \code{cex.top.axis} is the \code{cex} value
    for the top axis values.  \code{cex.main} is the \code{cex} value for
    the main title.  \code{cex.xbar} and \code{cex.y} are the \code{cex}
    values for the horizontal and vertical axes of the plot.
    \code{cex.left.axis} and \code{cex.pb.axis} are the \code{cex} values
    for the power or beta (Type II error) values and the \eqn{\mu_1} value
    in the power and beta plots.  \code{cex.xlab}, \code{cex.ylab}, and
    \code{cex.strip} are the \code{cex} values for \code{xlab},
    \code{ylab}, and \code{strip} labels.
  }
\item{key.axis.padding}{tuning constant to create additional room above the
  graph for a larger \code{cex.main} to fit.}
  \item{prob.labels}{logical. If \code{TRUE} label the floating
    probability values with their name, such as \eqn{\alpha}.  If
    \code{FALSE},
    then don't label them.  The default is \code{TRUE} for
    \code{type="hypothesis"}
    and \code{FALSE} for \code{type="confidence"}.
  }
  \item{xhalf.multiplier, yhalf.multiplier}{
    Numerical tuning constants to control the width and height of the floating
    probability values.  Empirically, we need a smaller value for the
    \pkg{shiny}
    app then we need for direct writing onto a graphic device.
  }
  \item{NTmethod}{Character string used when \code{shiny=TRUE}.  It is
    normally calculated by the methods.  \code{NTmethod} tells
    \code{shiny} how to use or ignore the \code{df} and \code{n}
    sliders.

    \code{"htest"} objects by default are interpreted
    as a single observation (\code{n=1}) of a \eqn{t}-statistic with
    \code{df} degrees of freedom.  The slider will let the user change
    the \code{df}, but not the \code{n}.

    \code{"power.htest"} objects are interpreted as a set of \eqn{n}
    obervations per group and \code{df} is calculated as \eqn{(n-1)} for
    single-sample tests and as \eqn{2(n-1)} for two-sample tests.
    The slider will let the user change \code{n} and will calculate the
    revised \code{df}.

    For the normal approximation to the binomial
    (\code{distribution.name="binomial"}),
    only \code{n} is meaningful.  The \code{df} is always ignored.

    For the default situation of \code{t}, determined by the initially
    specified sample size \eqn{n>1}, the degrees of freedom is
    calculated as \eqn{(n-1)} for single-sample tests and as
    \eqn{2(n-1)} for two-sample tests.  The default \code{z}, is
    initially specified by a sample size \eqn{n=1}.

    In all cases except the \code{"binomial"}, the user can change the
    interpretation of the \code{n} and \code{df} sliders.  The
    interpretation when both \code{n} and \code{df} are under user
    control is not always obvious.
}
\item{power, beta}{Logical.  If \code{TRUE}, then display that graph,
  else don't display it.  Passed forward to
  \code{\link{powerplot}}.
}
}
\details{
The graphs produced by this single function cover most of the first semester
introductory Statistics course.  The \code{htest} method plots the
results of the \code{stats::t.test} function.

\code{NormalAndTplot} is built on \code{\link[lattice]{xyplot}}.
Most of the arguments detailed in \code{xyplot} documentation work to
control the appearance of the plot.
}
\value{
\code{"trellis"} object.
}
\seealso{\code{\link{NTplot}}}
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
  grew out of discussions that Erich and I have had since the book was
  published.
  The method for \code{"htest"} objects incorporates ideas that Jay Kerns and I developed at the 2011 UseR! conference.
This version incorporates some ideas suggested by Moritz Heene.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\examples{
   NTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5))
   NTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5), distribution.name="t", df=4)
   NTplot(mean0=100, sd=12, mean1=113,  xbar=105,  xlim=c(92, 120), n=20)
   NTplot(mean0=100, sd=12, mean1=113,  xbar=105,  xlim=c(92, 120), n=20,
          zaxis=TRUE, z1axis=TRUE)
   NTplot(mean0=100, sd=12,  xbar=105,  xlim=c(92, 108), n=20, ntcolors="stoplight")
   NTplot(xbar=95, sd=10, xlim=c(65, 125), type="confidence",
          alpha.left=.025, alpha.right=.025)


x <- rnorm(12, mean=.78)
x.t <- t.test(x)
NTplot(x.t)
NTplot(x.t, type="confidence")
x.tg <- t.test(x, alternative="greater")
NTplot(x.tg)

y <- rnorm(12, mean=-.05)
xy.t <- t.test(x, y)
NTplot(xy.t)
NTplot(xy.t, type="confidence")

\dontrun{
     NTplot(shiny=TRUE)  ## with any other arguments for initialization of the shiny app.
}

\dontrun{
   ##  The partially transparent colors are:
   black127="#0000007F" ## HH:::ColorWithAlpha("black")
   green127="#00FF007F" ## HH:::ColorWithAlpha("green")
   blue127 ="#0000FF7F" ## HH:::ColorWithAlpha("blue")


## this is the default set of colors that are assigned when
## ntcolors="original" or when ntcolors is not specified
c(col.alpha = "blue",
  col.notalpha = "lightblue",
  col.beta = "red",
  col.power = "pink",
  col.pvalue = "green",
  col.pvaluetranslucent = green127,
  col.critical = "gray50",
  col.border = black127,
  col.text = "black",
  col.conf = "lightgreen")

  NTplot(                 )
  NTplot(mean1 = 2,       )
  NTplot(           xbar=1)
  NTplot(mean1 = 2, xbar=1)
  NTplot(type="confidence")


## this is the set of colors that are assigned when ntcolors="stoplight"
c(col.alpha = "red",
  col.notalpha = "honeydew2",
  col.beta = "orange",
  col.power = "pink",
  col.pvalue = "blue",
  col.pvaluetranslucent = blue127,
  col.critical = "gray50",
  col.border = black127,
  col.text = "black",
  col.conf = "lightgreen")

  NTplot(                   ntcolors="stoplight")
  NTplot(mean1 = 2,         ntcolors="stoplight")
  NTplot(           xbar=1, ntcolors="stoplight")
  NTplot(mean1 = 2, xbar=1, ntcolors="stoplight")
  NTplot(type="confidence", ntcolors="stoplight")

## this is the set of colors that are assigned when ntcolors="BW"
c(col.alpha             = "gray35",
  col.notalpha          = "gray85",
  col.beta              = "gray15",
  col.power             = "gray40",
  col.pvalue            = "gray50",
  col.pvaluetranslucent = HH:::ColorWithAlpha("gray65"),
  col.critical          = "gray15",
  col.border            = "gray75",
  col.text              = "black",
  col.conf              = "gray45")

  NTplot(                   ntcolors="BW")
  NTplot(mean1 = 2,         ntcolors="BW")
  NTplot(           xbar=1, ntcolors="BW")
  NTplot(mean1 = 2, xbar=1, ntcolors="BW")
  NTplot(type="confidence", ntcolors="BW")

}

\dontrun{
## mean1 and xbar
  NTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5))
  NTplot(mean0=0, mean1=-2, xbar=-1.8, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0)
  NTplot(mean0=0, mean1=2,  xbar=2.1,  xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025)
  NTplot(mean0=0, mean1=-2, xbar=-2.1, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025)

## mean1
  NTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5))
  NTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0)
  NTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025)
  NTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025)

## xbar
  NTplot(mean0=0, mean1=NA, xbar=1.8,  xlim=c(-3, 5))
  NTplot(mean0=0, mean1=NA, xbar=-1.8, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0)
  NTplot(mean0=0, mean1=NA, xbar=2.1,  xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025)
  NTplot(mean0=0, mean1=NA, xbar=-2.1, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025)

## t distribution
## mean1 and xbar
  NTplot(mean0=0, mean1=2,  xbar=1.8,  xlim=c(-3, 5),
         distribution.name="t", df=4)
  NTplot(mean0=0, mean1=-2, xbar=-1.8, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=2,  xbar=2.1,  xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=-2, xbar=-2.1, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## mean1
  NTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
         distribution.name="t", df=4)
  NTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=2,  xbar=NA, xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=-2, xbar=NA, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## xbar
  NTplot(mean0=0, mean1=NA, xbar=1.8,  xlim=c(-3, 5),
         distribution.name="t", df=4)
  NTplot(mean0=0, mean1=NA, xbar=-1.8, xlim=c(-5, 3),
         alpha.left=.05,  alpha.right=0, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=NA, xbar=2.1,  xlim=c(-3, 5),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)
  NTplot(mean0=0, mean1=NA, xbar=-2.1, xlim=c(-5, 3),
         alpha.left=.025, alpha.right=.025, distribution.name="t", df=4)

## confidence intervals

  NTplot(mean0=0, xlim=c(-3, 4), type="confidence")
  NTplot(xbar=01, xlim=c(-3, 4), type="confidence")
  NTplot(mean0=0, xlim=c(-4, 3), type="confidence",
         alpha.left=.05,  alpha.right=0)
  NTplot(mean0=0, xlim=c(-3, 3), type="confidence",
         alpha.left=.025, alpha.right=.025)
  NTplot(mean0=95, sd=10, xlim=c(65, 125), type="confidence",
         alpha.left=.025, alpha.right=.025)
  NTplot(mean0=95, sd=10, xlim=c(65, 125), type="confidence",
         alpha.left=.025, alpha.right=.025,
         distribution="t", df=10)

}
}
