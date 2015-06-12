\name{col3x2}
\alias{col3x2}
\docType{data}
\title{
col3x2 color dataset
}
\description{
col3x2 color dataset.
}
\usage{data("col3x2")}
\format{
  The format is:
 chr [1:6] "#1B9E77" "#D95F02" "#7570B3" "#66C2A5" "#FC8D62" "#8DA0CB"
}
\details{
3x2 color scheme.
  These colors look like a 3x2 color array when run through
  the vischeck simulator to see how they look for the three most
  common color vision deficiencies: Deuteranope, Protanope, Tritanope.
}
\references{
About 10\% of the population have color deficient vision.
Your job is make your graphs legible to everyone.
Download \code{ImageJ} from \url{http://rsb.info.nih.gov/ij/} and
\code{VischeckJ} from
\url{http://vischeck.com} and follow the instructions in those sites.
This program will allow you to simulate color deficient vision on your computer.

On my Mac, I need to doubleclick ij.jar to open the program.  Then open
the "Vischeck Panel" on the Plugins menu and navigate to a png file with
the "File Open" menu.  Click on each of the three types of color
deficiency.
}
\examples{
data(col3x2)
\dontrun{
library(RColorBrewer)
library(grid)
library(lattice)
col3x2 <- c(brewer.pal(n=3, "Dark2"), brewer.pal(n=3, "Set2"))
save(col3x2, file="col3x2.rda")
##
grid.newpage()
draw.key(list(text=list(c("Dark2", "Set2")),
              rectangles=list(col=col3x2[c(1,4,2,5,3,6)],
                              cex=1.5, size=8, height=.7, border=FALSE),
              columns=3,
              between=1,
              padding.text=8),
         draw=TRUE)
}
}
\keyword{datasets}
