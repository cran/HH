\name{LikertPercentCountColumns}
\alias{LikertPercentCountColumns}

\title{Display likert plots with percents in the first column of panels
  and counts in the second column of panels.}

\description{Display likert plots with percents in the first column of
  panels and counts in the second column of panels.  Order the rows
  either in their original order or by the positive order of the percent
  display.}

\usage{
LikertPercentCountColumns(
           x, data,
           px=list( ## defaults designed for long QuestionName values
             LL=c(.00,  .50), ## and 7in x 7in window
             LP=c(.50,  .70),
             ML=c(.50,  .51),  ## arbitrary, visually center the labels and legend
             RP=c(.71,  .87),
             RL=c(.87, 1.00)),
           ...,
           QuestionName="Question",
           as.percent="Capture and then ignore this argument",
           positive.order=FALSE)
}

\arguments{
  \item{x, data, positive.order}{formula, data.frame, Logical.
    See \code{\link{likert}}.}
  \item{\dots}{other arguments that can be used for \code{\link{likert}}.}
  \item{px}{See \code{\link{as.TwoTrellisColumns5}}.}
  \item{as.percent}{Capture this argument and ignore it.  The
    \code{as.percent} argument of \code{\link{likert}} will be
    \code{TRUE} in the left (Percent) column of the resulting
    \code{"TwoTrellisColumns5"} object and \code{FALSE} in the right
    (Count) column.}
  \item{QuestionName}{Character string containing the name of the
    column in \code{data} containing the values of the response variable.}
}


\value{A \code{"TwoTrellisColumns5"} object, consisting of a list
  containing the constructed left, middle, and right trellis objects,
  and an attribute containing the \code{px} value.
  See \code{\link{as.TwoTrellisColumns5}} for details.
}

\seealso{\code{\link{likert}}}

\examples{
library(HH)

## These are based on the Professional Challenges example in ?likert
data(ProfChal)

levels(ProfChal$Subtable)[6] <- "Prof Recog" ## reduce length of label

## See ?print.TwoTrellisColumns for this example using the original ordering

## Order both the plot of the count plot and the percent plot by the
## positive.order of the percent plot.

LikertPercentCountColumns(Question ~ . | Subtable, ProfChal,
                          layout=c(1,6), scales=list(y=list(relation="free")),
                          ylab=NULL, between=list(y=0),
                          strip.left=strip.custom(bg="gray97"), strip=FALSE,
                          par.strip.text=list(cex=.7),
                          positive.order=TRUE,
                          main="Is your job professionally challenging?")
\dontrun{
## Retain original order of the Question variable

LikertPercentCountColumns(Question ~ . | Subtable, ProfChal,
                          layout=c(1,6), scales=list(y=list(relation="free")),
                          ylab=NULL, between=list(y=0),
                          strip.left=strip.custom(bg="gray97"), strip=FALSE,
                          par.strip.text=list(cex=.7),
                          main="Is your job professionally challenging?")

## Order both the plot of the count plot and the percent plot by the
## positive.order of the percent plot.
## Just the "Employment sector".
LPCCEs <-
LikertPercentCountColumns(Question ~ . ,
                          ProfChal[ProfChal$Subtable == "Employment sector", -7],
                          ylab=NULL, between=list(y=0),
                          par.strip.text=list(cex=.7),
                          positive.order=TRUE,
                          main="Is your job professionally challenging?\nEmployment sector",
                          px=list( ## defaults designed for long QuestionName values
                            LL=c(.00,  .50), ## and 7in x 7in window
                            LP=c(.49,  .70),
                            ML=c(.50,  .51),  ## arbitrary, visually center the labels and legend
                            RP=c(.71,  .84),
                            RL=c(.87, 1.00)))
LPCCEs$RP$x.scales$at <- c(0,100,200)
LPCCEs$RP$x.scales$labels <- c(0,100,200)
LPCCEs
}
}

\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{hplot}
