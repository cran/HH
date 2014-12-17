\name{NormalAndT.power}
\alias{NormalAndT.power}
\alias{NormalAndT.and.power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a power graph based on the NormalAndT plot.
}
\description{
Construct a power graph based on the NormalAndT plot.
}
\usage{
NormalAndT.power(nt,
                   which=c("power","beta"),
                   digits=4,
                   digits.top.axis=digits, digits.left=digits,
                   col.power=attr(nt, "color")["col.power"],
                   cex.top.axis=1, cex.left.axis=1,
                   lwd.reference=4, lwd.line=2,
                   main=which)
NormalAndT.and.power(nt, which="power",
                       pnt=NormalAndT.power(
                         nt,
                         digits.top.axis=digits.top.axis,
                         digits.left=digits.left,
                         which=which,
                         cex.top.axis=cex.top.axis, cex.left.axis=cex.left.axis,
                         lwd.reference=4, lwd.line=2),
                       digits=4, digits.top.axis=digits, digits.left=digits,
                       cex.top.axis=1, cex.left.axis=1,
                       display.ylab=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nt}{
A \code{"NormalAndT"} object from \code{\link{NormalAndTplot}}.
}
\item{which}{Which graph is to be displayed? \code{"power"} for the
  power curve, or \code{"beta"} for the operating characteristic curve.
}
\item{digits.top.axis, digits.left, digits, cex.top.axis, cex.left.axis}{
    \code{digits.top.axis} is the number of significant digits for the top
    axis.  \code{digits.left} is the number of significant digits for
    the observed power or beta on the left axis.  \code{digits} is a
    convenience argument to set both \code{digits.axis} and
    \code{digits.left} at the same time.  These number is passed to the
    \code{\link[base]{format}} function. \code{cex.top.axis} is the \code{cex} value
    for the top axis values.  \code{cex.left.axis} is the \code{cex} value for
    the observed power or beta on the left axis.
}
\item{col.power}{Color used for the crosshairs on the power or beta
  panel.
  The default value is the color used for the power region of the
  \code{NormalAndT} panel.
}
\item{lwd.reference, lwd.line}{\code{lwd} values for the power or beta
  panel.
  \code{lwd.line} is used for the power curve or beta curve.
  \code{lwd.reference}
  is used for the crosshairs.
}
\item{pnt}{A \code{"trellis"} object produced by \code{NormalAndT.power}.
}
\item{display.ylab}{logical.  Should the \code{ylab} for the panels be displayed?
}
\item{main}{Main title for graph.
}
}
\details{
  \code{NormalAndT.power} constructs a power graph from
  the \code{"NormalAndT"} object.
  \code{NormalAndT.and.power} calls \code{NormalAndT.power} to
  construct a power graph from the \code{"NormalAndT"} object, and then
  catenates it with the \code{\link[latticeExtra]{c.trellis}} function to
  the original \code{"NormalAndT"} object.
}
\value{
\code{"trellis"} object.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\examples{

nt <- NormalAndTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1))
NormalAndT.and.power(nt)

\dontrun{
tt <- NormalAndTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1), df=4, distribution.name="t")
NormalAndT.and.power(tt)

ntc <- NormalAndTplot(xbar=2, sd=3, n=20, xlim=c(-.1, 4.1), type="confidence",
                    alpha.left=.025, alpha.right=.025)
ntc
try(NormalAndT.and.power(ntc))
}
}
\keyword{ hplot }
