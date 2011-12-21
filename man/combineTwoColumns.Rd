\name{combineTwoTrellisColumns}
\alias{combineTwoTrellisColumns}
\alias{LikertPercentCountColumns}
\title{Combine two trellis objects, each containing one column of
  trellis panels, into a single trellis object containing all panels.}
\description{Combine two trellis objects, each containing one column of
  trellis panels, into a single
  trellis object containing all panels.  This is designed specifically
  for displaying likert plots with percents in the first column and
  counts in the second column.
}
\usage{
LikertPercentCountColumns(x, w=c(3,1), ...,
                          strip.lines=.8, strip.cex=.8)

combineTwoTrellisColumns(LLLRRR, w=c(1,1), strip.lines=.8, strip.cex=.7, ...)
}
\arguments{
  \item{x}{Object of class \code{"listOfNamedMatrices"}, suitable for
    use with the \code{plot.likert} function.}
  \item{w}{\code{w} argument to \code{\link[latticeExtra]{resizePanels}}.}
  \item{\dots}{other arguments that can be used to update a \code{"trellis"} object.}
  \item{strip.lines, strip.cex}{abbreviated access to
    \code{par.strip.text$lines} and \code{par.strip.text$cex}.}
  \item{LLLRRR}{two-row matrix of \code{"trellis"} objects.  The first/second
    row of \code{LLLRRR} contains the first/second column of the resulting
    trellis object.
    The row names of each pair of \code{"trellis"} objects must be the same.}
}
\value{A \code{"trellis"} object.}
\details{Changes component values of the  \code{"trellis"} object using manipulations
  based on those in the \code{\link[latticeExtra]{resizePanels}} package.
}
\seealso{\code{\link{likert}}}
\examples{
## These are based on the Professional Challenges example in ?likert
library(HH)
data(ProfChal)

## Change name in the "Attitude" panel
names(ProfChal)[6] <- "Prof Recog"
##
## normal usage for percents and counts displayed jointly
LikertPercentCountColumns(ProfChal,
                          main="Is your job professionally challenging?",
                          sub="LikertPercentCountColumns test, 9x8 window")
##
## Restore original name
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## Same example with details illustrated for two displays with coordinated x-axis
## labels within each pair of panels.
##
LLL <- sapply(ProfChal[6:1], plot.likert, as.percent="noRightAxis", xlab="Percent", simplify=FALSE)
RRR <- sapply(ProfChal[6:1], plot.likert, rightAxis=TRUE, xlab="Count", simplify=FALSE)
LLLRRR <- rbind(Percent=LLL, Count=RRR) ## rbind is needed
dimnames(LLLRRR)[[2]][1] <- "Prof Recog"  ## original is "Attitude\ntoward\nProfessional\nRecognition"
combineTwoTrellisColumns(LLLRRR, w=c(3,1),
                         main="Is your job professionally challenging?",
                         sub="combineTwoTrellisColumns test")
}
\author{Richard M. Heiberger}
\keyword{hplot}
