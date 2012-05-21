\name{panel.cartesian}
\alias{panel.cartesian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{trellis panel function, with labeled rows and columns and without
strip labels.}
\description{
trellis panel function, with labeled rows and columns and without
strip labels.  Designed for use with the ladder of powers plot.
}
\usage{
panel.cartesian(x, y,
                x.label=unique(panel.labels[,"x"]),
                y.label=unique(panel.labels[,"y"]),
                group.label.side="",
                axis3.line=1,
                xg.label, yg.label, g.cex=.7,
                rescale=list(x=TRUE,y=TRUE), ...,
                browser.on=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{x and y as for any other panel function}
  \item{x.label}{labels for the columns of the scatterplot matrix}
  \item{y.label}{labels for the rows of the scatterplot matrix}
  \item{axis3.line}{The \code{x.label} doesn't always show up in the
right place.  This allows the user to adjust it's position. }
  \item{group.label.side}{\code{c("","left","top")}, when the plotting
formula is conditioned on a group factor, the levels of the group are
displayed in the margins of the plot.  The appearance depends on the
setting of the trellis \code{between} argument.  Getting it to look good
for any given plot requires experimentation.  Since it is redundant with
the information in the strip labels, leaving it at the default \code{""}
is often the best thing to do.}
  \item{xg.label}{group labels for rows of the scatterplot matrix}
  \item{yg.label}{group labels for rows of the scatterplot matrix}
  \item{g.cex}{\code{cex} for the group labels}
  \item{rescale}{alternate way to get something similar to \code{relation="free"}}
  \item{\dots}{other arguments}
  \item{browser.on}{logical, normally \code{FALSE}.  This is a debugging
tool.  When \code{TRUE}, the \code{browser()} is turned on at various
critical points.}
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\seealso{\code{\link{ladder}}, \code{\link{xysplom}}}
\examples{
data(rent)  ## Weisberg's file alr162

rent.lm <- lm(rnt.alf ~ rnt.till + cow.dens + lime, data=rent)
rent$resid.rent <- resid(rent.lm)

xysplom(resid.rent ~ rnt.till + cow.dens | lime, data=rent,
        layout=c(2,2))

xysplom(resid.rent ~ rnt.till + cow.dens | lime, data=rent,
        layout=c(2,2),
        xlab="", ylab="",
        x.label="", y.label="",
        group.label.side="",
        par.strip.text=list(cex=1.2),
        panel=panel.cartesian,
        axis3.line=2.4,
        scales=list(
          relation="same",
          alternating=FALSE, labels=FALSE, ticks=FALSE),
        between=list(x=1, y=3))

xysplom(resid.rent ~ rnt.till + cow.dens | lime, data=rent,
        layout=c(2,2),
        xlab="", ylab="",
        x.label="", y.label="",
        group.label.side="",
        par.strip.text=list(cex=1.2),
        panel=panel.cartesian,
        axis3.line=3.6,
        scales=list(
          relation="same",
          alternating=FALSE, labels=FALSE, ticks=FALSE),
        rescale=list(x=FALSE, y=FALSE),
        between=list(x=1, y=3))

xysplom(resid.rent ~ rnt.till + cow.dens | lime, data=rent,
        layout=c(2,2),
        xlab="", ylab="",
        x.label="", y.label="",
        group.label.side="",
        par.strip.text=list(cex=1.2),
        panel=panel.cartesian,
        axis3.line=3.6,
        scales=list(
          relation="free",
          alternating=FALSE, labels=FALSE, ticks=FALSE),
        between=list(x=1, y=3))

tmp <-
xysplom(resid.rent ~ rnt.till + cow.dens | lime, data=rent,
        layout=c(2,2),
        xlab="", ylab="",
        y.label="resid",
        group.label.side="top",
        par.strip.text=list(cex=1.2),
        panel=panel.cartesian,
        axis3.line=3.6,
        scales=list(alternating=FALSE, labels=FALSE, ticks=FALSE),
        rescale=list(x=FALSE, y=FALSE),
        between=list(x=4, y=5))
if.R(r=tmp$par.settings <- list(layout.widths=list(right.padding=4)),
     s={})
tmp

}
\keyword{hplot}% at least one, from doc/KEYWORDS
