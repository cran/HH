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
library(grid)
library(lattice)
Discrete4 <- brewer.pal(n=12, "Paired")[c(6,2,5,1)]
Discrete4
save(Discrete4, file="Discrete4.rda")
##
grid.newpage()
draw.key(list(text=list(c("Discrete4")),
              rectangles=list(col=Discrete4,
                              cex=1.5, size=8, height=.7, border=FALSE),
              columns=4,
              between=1,
              padding.text=8),
         draw=TRUE)
}
}
\keyword{datasets}
