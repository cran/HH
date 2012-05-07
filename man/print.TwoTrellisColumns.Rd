\name{print.TwoTrellisColumns}
\alias{print.TwoTrellisColumns}
\alias{print.TwoTrellisColumns5}
\alias{as.TwoTrellisColumns}
\alias{as.TwoTrellisColumns5}
\alias{emptyLeftAxisLeftStrip}

\alias{leftLabels.trellis}
\alias{rightLabels.trellis}
\alias{panelOnly.trellis}
\alias{mainSubLegend.trellis}
\alias{emptyRightAxis}

\title{Print two conformable trellis plots in adjacent columns with user
  control of widths.}
\description{Print two conformable trellis plots in adjacent columns with user
  control of widths.  Left y tick-labels and left.strip are removed from the
  right-hand plot.}
\usage{
as.TwoTrellisColumns(left,  ## left  is the left trellis object
                     right, ## right is the right trellis object
                     ## Both left and right must have identical
                     ## settings for number and size of vertical panels,
                     ## left-axis labels, number of lines in main, sub, legend
                     ## and maybe some other items
                     ...,
                     panel.width=.48,
                     px=list(
                       L=c(0, panel.width),
                       R=c(1-panel.width, 1),
                       M=c(panel.width, 1-panel.width)),
                     keepLegend=TRUE)

as.TwoTrellisColumns5(left,  ## left  is the left trellis object
                      right, ## right is the right trellis object
                      ## Both left and right must have identical
                      ## settings for number and size of vertical panels,
                      ## left-axis labels, number of lines in main, sub, legend.
                      ...,
                      pw=c(.3, .30, .01, .30, .09),
                      px=list(
                        LL=c(0, pwc[1]),
                        LP=pwc[1:2],
                        ML=pwc[2:3],
                        RP=pwc[3:4],
                        RL=pwc[4:5]),
                      pwc=cumsum(pw)
                      )
\method{print}{TwoTrellisColumns}(x, px=attr(x, "px"), ...)
\method{print}{TwoTrellisColumns5}(x, px=attr(x, "px"), ...)

emptyLeftAxisLeftStrip(x)
leftLabels.trellis(x)
rightLabels.trellis(x)
panelOnly.trellis(x)
mainSubLegend.trellis(x)
emptyLeftAxisLeftStrip(x)
emptyRightAxis(x)
}

\arguments{
  \item{left, right}{Conformable \code{"trellis"} objects.
    Both must have the identical settings for number and size of
    vertical panels, left-axis labels, number of
    lines in main, sub, legend.}
\item{panel.width}{Numeric scalar between 0 and 0.5. Common width of left and right panels. The default
value .48 value works well for the \code{USAge.table} example.
This number is expanded in the \code{px} argument to the \code{x} values
used in the \code{position} argument of the \code{\link[lattice]{print.trellis}} function.}
\item{px}{\code{x} values used in the position argument of the
\code{print.trellis} function.
The default makes the Left and Right panels the same width and the
Middle containing the y-axis is given the remainder.  Overlapping is
permitted.
The appearance depends on the width of the graphics device.
The user must determine which values of \code{px} give the best appearance.}
\item{keepLegend}{If \code{TRUE} and \code{x} contains a bottom legend,
then it is printed along with
the Middle section containing the y-axis.
If \code{FALSE} or there is no bottom legend, then the bottom legend is not printed.}
\item{\dots}{Other arguments are ignored.}
\item{x}{A \code{"TwoTrellisColumns"} object which will be printed.} 
\item{pw, pwc}{\code{pw} vector of five positive numbers that sum to 1.  These
  are the relative widths of the five sections of the result: LeftLabels,
  LeftPanel, MainSubLegend, RightPanel, RightLabels.  \code{pwc} is
  the cumulative sum of \code{pw}.
  \code{pwc} is expanded in the \code{px} argument to the \code{x} values
used in the \code{position} argument of the \code{\link[lattice]{print.trellis}} function.
}
}

\details{\code{as.TwoTrellisColumns} works by stripping the title, subtitle, and
bottom legend from the left and right arguments.  It removes the left
tick-labels and the \code{strip.left}
from the right argument.  It constructs a middle object and places the
title, subtitle, and bottom legend on the middle argument.

\code{print.TwoTrellisColumns} is a print method for two conformable
\code{trellis} objects.  The three objects are printed on the same graph window using the
\code{px} values as the x-position arguments for the
\code{print.trellis} function.

\code{emptyLeftAxisLeftStrip,
leftLabels.trellis,
rightLabels.trellis,
panelOnly.trellis,
mainSubLegend.trellis,
emptyLeftAxisLeftStrip,
emptyRightAxis} blanks out the various components of the trellis
argument and retains their vertical spacing.
}
\value{A \code{"TwoTrellisColumns"} object, consisting of a list
  containing the constructed left, middle, and right trellis objects,
  and an attribute containing the \code{px} value.}

\seealso{\code{\link{likert}} for the details on the motivating example.}
\examples{
library(HH)
data(ProfChal)

## Putting both percents and counts on the same plot requires
## coordination.  The easiest way is to order the original tables of
## counts by the order of the percent plot.

names(ProfChal)[6] <- "Prof Recog" ## reduce length of label

percentPlot <- likert(ProfChal, as.percent="noRightAxis", positive.order=TRUE,
                      main="Is your job professionally challenging?",
                      strip.left.par=list(cex=.7))
## percentPlot
pct.order <- percentPlot$y.limits

ProfChal.pct.order <- sapply(seq(along=ProfChal),
                           function(i, mylist, myorder)
                             {mylist[[i]][rev(myorder[[i]]),,drop=FALSE]},
                           ProfChal, pct.order,
                           simplify=FALSE)
names(ProfChal.pct.order) <- names(ProfChal)
class(ProfChal.pct.order) <- class(ProfChal)

countPlot   <- likert(ProfChal.pct.order, rightAxis=TRUE,
                      scales=list(x=list(at=c(0, 250, 500))),
                      main="Is your job professionally challenging?")
## countPlot
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition" ## Restore original label

as.TwoTrellisColumns(percentPlot, countPlot,
                     px=list(L=c(0,.77),   ## left labels and left panel    ## Overlap is ok.
                             M=c(.60,.61), ## main, sub, and legend of left ## Do it visually for
                             R=c(.71,1)))  ## right panel and right labels  ## the graphics window width.

as.TwoTrellisColumns5(percentPlot, countPlot)  ## bad overlap for this example with 7in x 7in window

as.TwoTrellisColumns5(percentPlot, countPlot, ## acceptable in 7in x 7in
                     pw=c(.528, .186, .01, .186, .09)) ## These 5 numbers must sum to 1.

as.TwoTrellisColumns5(percentPlot, countPlot, ## better in 7in x 7in
                     px=list(
                       LL=c(0.000, 0.528),  ## 0.528 makes LL and LP touch without overlap        ## left labels and strip.left
                       LP=c(0.528, 0.714),                                                        ## left panel
                       ML=c(0.580, 0.581),  ## shifted left, to center main and key               ## main, sub, key from left argument
                       RP=c(0.724, 0.910),  ## gap from 0.714 to 0.724 puts space between columns ## right panel
                       RL=c(0.920, 1.000))) ## extra space                                        ## right labels
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{hplot}
