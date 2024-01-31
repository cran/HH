\name{residual.plots.lattice}
\alias{residual.plots.lattice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Construct four sets of regression plots: Y against X, residuals
  against X, partial residuals against X, partial residuals against each
  X adjusted for all the other X columns.

}
\description{
  Construct four sets of regression plots.
  Response variable $Y$ against each $X_j$, residuals $e$ against each $X_j$,
partial residuals plots of $e^j$ against each $X_j$, added variable plots
of $e^j$ against the residuals of each $X_j$ adjusted for
the other $X$ columns.
The slopes shown in the panels of both bottom rows are equal to the
regression coefficients.
}
\usage{
residual.plots.lattice(lm.object, X=dft$x, layout=c(dim(X)[2],1),
                       par.strip.text=list(cex=.8),
                       scales.cex=.6,
                       na.action=na.pass,
                       y.relation="same",
                       ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.object}{
\code{lm.object}
}
  \item{X}{
Identify the variables in each of the x, y, group positions in a formula object.
See \code{\link{do.formula.trellis.xysplom}} for more detail.
}
  \item{layout, par.strip.text, \dots}{
\pkg{lattice} arguments.  See \code{\link{xyplot}}.
}
  \item{scales.cex}{
\code{cex} for the \code{scales} argument in \code{\link{xyplot}}.
}
  \item{na.action}{
See \code{\link{na.action}}.
}
  \item{y.relation}{
\code{relation} for the \code{y} argument to \code{scales} argument in \code{\link{xyplot}}.
}
}
\value{
\code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{residual.plots}}, \code{\link{print.latticeresids}}}
\examples{
     data(longley)
     longley.lm <- lm( Employed ~ . , data=longley, x=TRUE, y=TRUE)
     residual.plots.lattice(longley.lm)

     \dontrun{
       longleyResid <- tempfile("longleyResid", fileext = ".pdf")
       pdf(longleyResid, height=9, width=14)
       print(residual.plots.lattice(longley.lm, pch=19),
             A4.left=.0125, panel.width=list(5,"cm"))
       dev.off()
     }
     }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
