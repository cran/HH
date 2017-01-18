\name{NormalAndTPower}
\alias{NormalAndTPower}
\alias{powerplot}
\alias{powerplot.NormalAndTplot}
\alias{power curve}
\alias{beta curve}
\alias{operating characteristic curve}
%\alias{NormalAndTAndPower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a power graph based on the NTplot.
}
\description{
Construct a power graph based on the \code{NTplot}.  The exported function
\code{powerplot} calls \code{NormalAndTPower} to construct a power curve or beta curve (operating
characteristic curve) (or both) from its argument and catenates it to the original
graph.  The unexported function \code{NormalAndTPower} does the
construction.
}
\usage{
powerplot(nt, ...)

\method{powerplot}{NormalAndTplot}(nt, power=TRUE, beta=FALSE, ...,
                         hh=if (power && beta) c(6,2,2) else c(6,2))

NormalAndTPower(nt,
                which=c("power","beta"),
                digits=4,
                digits.top.axis=digits, digits.left=digits,
                col.power=attr(nt, "color")["col.power"],
                col.beta=attr(nt, "color")["col.beta"],
                cex.pb.axis=1, cex.left.axis=1, cex.xbar=1,
                lwd.reference=4, lwd.line=2,
                main=which, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nt}{For the generic \code{powerplot}, an object.
For the \code{NormalAndTplot} method, a \code{"NormalAndTplot"} object from \code{\link{NTplot}}.
}
\item{power, beta}{Logical.  If \code{TRUE}, then display that graph,
  else don't display it.  Used by \code{powerplot}.
}
\item{which}{Which graph is to be displayed? \code{"power"} for the
  power curve, or \code{"beta"} for the operating characteristic curve.
  Used by \code{NormalAndTPower}.
}
\item{\dots}{Additional arguments passed on to methods.}
\item{hh}{The \code{h} argument for \code{\link[latticeExtra]{resizePanels}}.}
\item{digits.top.axis, digits.left, digits, cex.pb.axis, cex.left.axis, cex.xbar}{
    \code{digits.top.axis} is the number of significant digits for the top
    axis.  \code{digits.left} is the number of significant digits for
    the observed power or beta on the left axis.  \code{digits} is a
    convenience argument to set both \code{digits.axis} and
    \code{digits.left} at the same time.  These number is passed to the
    \code{\link[base]{format}} function. \code{cex.top.axis} is the \code{cex} value
    for the top axis values.  \code{cex.left.axis} is the \code{cex} value for
    the observed power or beta on the left axis.   \code{cex.xbar} is
    the \code{cex} value for the horizontal axis.
}
\item{col.power, col.beta}{Colors used for the crosshairs on the power and beta
  panels.
  The default values are the colors used for the power and beta regions of the
  \code{NTplot} panel.
}
\item{lwd.reference, lwd.line}{\code{lwd} values for the power or beta
  panel.
  \code{lwd.line} is used for the power curve or beta curve.
  \code{lwd.reference}
  is used for the crosshairs.
}
\item{main}{Main title for graph.
}
}
\value{
\code{"trellis"} object.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\examples{

nt <- NTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1), xbar=3.5)
powerplot(nt)

\dontrun{
tt <- NTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1), xbar=3.5, df=4, distribution.name="t")
powerplot(tt)

ntc <- NTplot(xbar=2, sd=3, n=20, xlim=c(-.1, 4.1), type="confidence",
              alpha.left=.025, alpha.right=.025)
ntc
try(powerplot(ntc))
}
}
\keyword{ hplot }
