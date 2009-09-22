\name{panel.dotplot.tb}
\alias{panel.dotplot.tb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dotplot with evenly spaced tiebreakers.}
\description{
  Dotplot with evenly spaced tiebreakers.
  Multiple hits on a specific x value are stacked.
}
\usage{
panel.dotplot.tb(x, y, factor=.1,
                 jitter.data=TRUE, horizontal=TRUE,
                 max.freq=max(sapply(subsets, length)),
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{See \code{\link[lattice]{xyplot}}.}
  \item{factor}{jitter factor, see \code{\link[lattice]{xyplot}}.
  Increment is \code{factor/max.freq} where \code{max.freq} is the
    maximum number of duplicates of any x value in any y group.}
  \item{jitter.data, horizontal}{Always \code{TRUE}.}
  \item{max.freq}{maximum number of observation at any combination of
    response values, factor levels, and group levels.
    If the formula includes one or more conditioning factors, then the
    user is responsible for providing a value for \code{max.freq}.}
  \item{\dots}{Other arguments for \code{\link[lattice]{xyplot}}.}
}
\details{
Creates (possibly grouped) Dotplot of x against y.  y is the `factor'.
}
\section{Warning }{ 
    If the formula includes one or more conditioning factors, then the
    user is responsible for providing a value for \code{max.freq}.
    The default behavior is a different \code{max.freq} for each panel
    in a multi-panel display.  
}
\author{
  Richard M. Heiberger

Maintainer: Richard M. Heiberger <rmh@temple.edu>
}
\examples{
x <- c(1,1,2,2,2,5,4,2,1,5)
y <- factor(letters[rep(1:2, 5)])

dotplot(x, panel=panel.dotplot.tb)
dotplot(x, panel=panel.dotplot.tb, factor=.2)
dotplot(y ~ x, panel=panel.dotplot.tb)
dotplot(y ~ x, panel=panel.dotplot.tb, cex=1.5, factor=.15)



quiz <- data.frame(scores=sample(10, 360, replace=TRUE),
                   date=rep(rep(c("0902", "0916", "0930"), c(40,40,40)), 3),
                   section=rep(
                     c("Stat1-3", "Stat1-5", "Stat1-8"),
                     c(120,120,120)))

dotplot(date ~ scores | section, data=quiz,
        panel=panel.dotplot.tb, factor=.5)

dotplot(date ~ scores | section, data=quiz,
        panel=panel.dotplot.tb, factor=.5,
        layout=c(1,3), between=list(y=1),
        main='Three quizzes for three sections of Stat 1')


## If the formula includes one or more conditioning factors, then the
## user is responsible for providing a value for the argument max.freq
##
a <- rep(1, 10)
z <- c(1,1,2,2,2,3,2,3,1,1)
g <- LETTERS[c(1,1,1,1,1,2,2,2,2,2)]

print(split=c(1,1,2,1), more=TRUE,
dotplot( a ~ z | g, panel=panel.dotplot.tb,
        factor=.6, cex=1.5, layout=c(2,1),
        main="different scaling in each panel")
)

print(split=c(2,1,2,1), more=FALSE,
dotplot( a ~ z | g, panel=panel.dotplot.tb, max.freq=3,
        factor=.6, cex=1.5, layout=c(2,1),
        main="same scaling in each panel")
)

}
\keyword{dplot}
