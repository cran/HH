\name{print.TwoTrellisColumns}
\alias{print.TwoTrellisColumns5}
\alias{as.TwoTrellisColumns5}
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
                      pwc=cumsum(pw),
                      strip.left=TRUE,
                      y.tck=c(0,0)
                      )

\method{print}{TwoTrellisColumns5}(x, px=attr(x, "px"), ...)

leftLabels.trellis(x)
rightLabels.trellis(x)
panelOnly.trellis(x, strip.left=FALSE, y.tck=0)
mainSubLegend.trellis(x)
emptyRightAxis(x)
}

\arguments{
  \item{left, right}{Conformable \code{"trellis"} objects.
    Both must have the identical settings for number and size of
    vertical panels, left-axis labels, number of
    lines in main, sub, legend.}
  \item{x}{\code{"trellis"} object.}
  \item{px}{These are used \code{x}-values used in the position argument
    of the \code{print.trellis} function.  The default (constructed from
    the \code{pw} argument) makes the Left and Right panels the same
    width and the Middle containing the y-axis is given the remainder.
    Overlapping is permitted.  The appearance depends on the width of
    the graphics device.}
  \item{pw, pwc}{\code{pw} vector of five positive numbers that sum to 1.  These
    are the relative widths of the five sections of the result: LeftLabels,
    LeftPanel, MainSubLegend, RightPanel, RightLabels.  \code{pwc} is
    the cumulative sum of \code{pw}.
    \code{pwc} is expanded in the \code{px} argument to the \code{x} values
    used in the \code{position} argument of the
    \code{\link[lattice]{print.trellis}} function.
  }
  \item{strip.left}{See \code{\link[lattice]{barchart}}.}
  \item{y.tck}{A vector of one or two numeric values.
    This will be used as the \code{y} \code{tck} value
    for the right column of panels.  See \code{'tck'} in
    \code{\link[lattice]{barchart}} for details.}
  \item{\dots}{Other arguments are ignored.}
}

\details{
\code{as.TwoTrellisColumns5} constructs a \code{"TwoTrellisColumns5"}
object, which is a list of five trellis objects named \code{"LL", "LP",
"ML", "RP", "RL"}.  \code{LL} is the left labels from the \code{left}
input object. \code{LP} is the panels from the \code{left} input object.
\code{ML} is the middle labels from the \code{left} object; these are
the \code{main} title, \code{sub} title, and \code{legend}.  \code{RP}
is the panels from the \code{right} input object.  \code{RL} is the
right labels from the \code{right} input object.

\code{print.TwoTrellisColumns5} is a print method for a
\code{"TwoTrellisColumns5"} object.  It takes left-to-right positioning
information from the \code{"px"} attribute of its argument \code{x} or
from an input argument.  The numbers are used as the \code{"x"}
information for the \code{position} argument to the \code{print.trellis}
method.

\code{emptyLeftAxis,
leftLabels.trellis,
rightLabels.trellis,
panelOnly.trellis,}\cr
\code{mainSubLegend.trellis,
emptyLeftStrip,
emptyRightAxis} are functions which blank out the various components of the trellis
argument and retains their vertical spacing.
}

\value{A \code{"TwoTrellisColumns5"} object, consisting of a list
  containing the constructed left, middle, and right trellis objects,
  and an attribute containing the \code{px} value.}

\seealso{\code{\link{likert}} for the details on the motivating example.}
\examples{
library(HH)


## These are based on the Professional Challenges example in ?likert
data(ProfChal)
levels(ProfChal$Subtable)[6] <- "Prof Recog" ## reduce length of label

## initial ordering of Question factor
PCC <- likert(Question ~ . | Subtable, ProfChal, ylab=NULL,
              rightAxis=TRUE,
              layout=c(1,6),
              strip=FALSE,
              strip.left=strip.custom(bg="gray97"),
              par.strip.text=list(cex=.7),
              scales=list(y=list(relation="free")),
              main="Is your job professionally challenging?")

## initial ordering of Question factor
PCP <- likert(Question ~ . | Subtable, ProfChal, ylab=NULL,
              as.percent=TRUE,
              layout=c(1,6),
              strip=FALSE,
              strip.left=strip.custom(bg="gray97"),
              par.strip.text=list(cex=.7),
              scales=list(y=list(relation="free")),
              main="Is your job professionally challenging?")

\dontrun{
## default equal widths of the two panels
as.TwoTrellisColumns5(PCP, PCC)  ## 11in x 7in

## make left panel twice as wide as right panel
as.TwoTrellisColumns5(PCP, PCC, pw=c(.3, .4, .01, .2, .09))  ## 11in x 7in
##                                   --------------------    ## sum to 1.00

## make left panel twice as wide as right panel, and control position of main and legend
as.TwoTrellisColumns5(PCP, PCC,  ## 11in x 7in
                      px=list(
                        LL=c(.00,  .30),
                        LP=c(.30,  .70),
                        ML=c(.60,  .61),  ## arbitrary,
                                          ## visually center the labels and legend
                        RP=c(.71,  .91),
                        RL=c(.91, 1.00)))
}

## Size that works in default 7x7 window.  7x7 is not recommended for
## this example because most of the space is used for labeling and not
## much for the panels containing the data.  Use the px values for the
## 11x7 illustrated above in the dontrun section.

as.TwoTrellisColumns5(PCP, PCC,  ## 7in x 7in
                      px=list(
                        LL=c(.00,  .50),
                        LP=c(.50,  .70),
                        ML=c(.50,  .51),  ## arbitrary,
                                          ## visually center the labels and legend
                        RP=c(.71,  .87),
                        RL=c(.87, 1.00)))

## Ordering the rows by the lengths of the positive bars and also
## put percents and counts on the same plot.
## The easiest way is to use the LikertPercentCountColumns function:

LikertPercentCountColumns(Question ~ . | Subtable, ProfChal,
                          layout=c(1,6), scales=list(y=list(relation="free")),
                          ylab=NULL, between=list(y=0),
                          strip.left=strip.custom(bg="gray97"), strip=FALSE,
                          par.strip.text=list(cex=.7),
                          positive.order=TRUE,
                          main="Is your job professionally challenging?")

\dontrun{

## Ordering the rows by the lengths of the positive bars and also
## putting percents and counts on the same plot requires coordination.
## The easiest way is to order the original tables of counts by the
## order of the percent plot.

percentPlot <- likert(Question ~ . | Subtable, ProfChal,
                      as.percent=TRUE,
                      layout=c(1,6), scales=list(y=list(relation="free")),
                      ylab=NULL, between=list(y=0),
                      strip.left=strip.custom(bg="gray97"), strip=FALSE,
                      par.strip.text=list(cex=.7),
                      positive.order=TRUE,
                      main="Is your job professionally challenging?")
## percentPlot
pct.order <- percentPlot$y.limits[[1]]

ProfChal2 <- ProfChal
ProfChal2$Question <- factor(ProfChal2$Question, levels=rev(pct.order))

countPlot   <- likert(Question ~ . | Subtable, ProfChal2,
                      layout=c(1,6),
                      rightAxis=TRUE,
                      scales=list(y=list(relation="free"),
                                  x=list(at=c(0, 250, 500))),
                      ylab=NULL, between=list(y=0),
                      strip.left=strip.custom(bg="gray97"), strip=FALSE,
                      par.strip.text=list(cex=.7),
                      main="Is your job professionally challenging?")
## countPlot
levels(ProfChal$Subtable)[6] <-
      "Attitude\ntoward\nProfessional\nRecognition" ## Restore original label

## Size that works in default 7x7 window.  7x7 is not recommended for
## this example because most of the space is used for labeling and not
## much for the panels containing the data.  Use the px values for the
## 11x7 illustrated above in the dontrun section.

as.TwoTrellisColumns5(percentPlot, countPlot,  ## 7in x 7in
                      px=list(
                        LL=c(.00,  .50),
                        LP=c(.50,  .70),
                        ML=c(.50,  .51),  ## arbitrary,
                                          ## visually center the labels and legend
                        RP=c(.71,  .87),
                        RL=c(.87, 1.00)))
}
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{hplot}
