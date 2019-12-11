\name{Discrete4}
\alias{Discrete4}
\docType{data}
\title{
Discrete with four levels color dataset.
}
\description{
  Discrete with four levels color dataset.
  These colors look like four distinct colors when run through
  the vischeck simulator to see how they look for the three most
  common color vision deficiencies: Deuteranope, Protanope, Tritanope.
}
\usage{data("Discrete4")}
\format{
  The format is:
 chr [1:4]  "#E31A1C" "#1F78B4" "#FB9A99" "#A6CEE3"
}
\details{
4x1 color scheme
}
\examples{
data(Discrete4)
\dontrun{
library(RColorBrewer)
library(lattice)
Discrete4 <- brewer.pal(n=12, "Paired")[c(6,2,5,1)]
Discrete4
## save(Discrete4, file="Discrete4.rda") ## data(Discrete4, package="HH")
##
barchart(~ 1:4, col=Discrete4, lwd=0, origin=0, horizontal=FALSE,
         xlab="Colors", scales=list(x=list(labels=Discrete4), y=list(labels=NULL)),
         main=paste("These colors look like four distinct colors when run through",
                    "the vischeck simulator to see how they look for the three most",
                    "common color vision deficiencies: Deuteranope, Protanope, Tritanope.",
                    sep="\n"))
}
}
\keyword{datasets}
