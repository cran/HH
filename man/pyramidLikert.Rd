\name{pyramidLikert}
\alias{print.pyramidLikert}
\alias{as.pyramidLikert}
\title{Print a Likert plot as a Population Triangle}
\description{Prints a likert plot in the traditional format for a
  population pyramid,
  with the Left and Right sides in separate panels, with the x-tick
  marks on the left side made positive, and with the y-axis in the Middle.
}
\usage{
\method{print}{pyramidLikert}(x, ...,
                     panel.width=.48,
                     px=list(
                       L=c(0, panel.width),
                       R=c(1-panel.width, 1),
                       M=c(panel.width, 1-panel.width)),
                    keepLegend=(length(x$legend$bottom$args$text) > 2),
                    xlab.top=list(
                      L=list(x$legend$bottom$args$text[1]),
                      R=list(x$legend$bottom$args$text[2]),
                      M=list(x$ylab, just=1)))

as.pyramidLikert(x, ...,
                 panel.width=.48,
                 px=list(
                   L=c(0, panel.width),
                   R=c(1-panel.width, 1),
                   M=c(panel.width, 1-panel.width)),
                 keepLegend=(length(x$legend$bottom$args$text) > 2),
                 xlab.top=list(
                   L=list(x$legend$bottom$args$text[1]),
                   R=list(x$legend$bottom$args$text[2]),
                   M=list(x$ylab, just=1)))
}
\arguments{
\item{x}{a single-panel \code{'trellis'} object.}
\item{\dots}{Other arguments are ignored.}
\item{panel.width}{Numeric scalar between 0 and 0.5. Common width of left and right panels. The default
value .48 value works well for the \code{USAge.table} example.
This number is expanded in the \code{px} argument to the \code{x} values
used in the \code{position} argument of the \code{\link[lattice]{print.trellis}} function.}
\item{px}{\code{x} values used in the position argument of the
\code{print.trellis} function.
The default makes the Left and Right panels the same width and the
Middle containing the y-axis is given the remainder.
}
\item{keepLegend}{If \code{TRUE} and \code{x} contains a bottom legend,
then it is printed along with
the Middle section containing the y-axis.
If \code{FALSE} or there is no bottom legend, then the bottom legend is not printed.}
\item{xlab.top}{A vector of three labels.  The default is designed for a
population triangle with two levels (usually, Male on one side and Female
on the other).  The Left and Right labels are taken from the first two labels in
the legend.  The Middle value is the variable name for the y-axis.}
}
\value{The input argument \code{x}.}
\details{
This is a print method for population triangles.  It is designed for a
likert plot with one left-side level and one right-side level.
It works for any single-panel \code{"trellis"} object, in the sense
that it produces a plot.
}
\seealso{\code{\link{likert}}}
\examples{
library(HH)
data(USAge.table)  ## from latticeExtra
USA79 <- USAge.table[75:1, 2:1, "1979"]/1000000
PL <- plot(as.likert(USA79),
                     main="Population of United States 1979 (ages 0-74)",
                     xlab="Count in Millions",
                     ylab="Age",
                     scales=list(
                       y=list(
                         limits=c(0,77),
                         at=seq(1,76,5),
                         labels=seq(0,75,5),
                         tck=.5))
                        )
PL
as.pyramidLikert(PL)

likert(USAge.table[75:1, 2:1, c("1939","1959","1979")]/1000000,
       main="Population of United States 1939,1959,1979 (ages 0-74)",
       sub="Look for the Baby Boom",
       xlab="Count in Millions",
       ylab="Age",
       scales=list(
         y=list(
           limits=c(0,77),
           at=seq(1,76,5),
           labels=seq(0,75,5),
           tck=.5)),
       strip.left=FALSE, strip=TRUE,
       layout=c(3,1), between=list(x=.5))


## For additional examples, see demo(PoorChildren, package="HH")

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{hplot}
