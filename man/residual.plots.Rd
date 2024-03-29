\name{residual.plots}
\alias{residual.plots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Residual plots for a linear model.}
\description{
  Residual plots for a linear model.  Four sets of plots are produced:
  (1) response against each of the predictor variables,
  (2) residuals against each of the predictor variables,
  (3) partial residuals for each predictor against that predictor
  ("partial residuals plots", and
  (4) partial residuals against the residuals of each predictor
  regressed on the other predictors ("added variable plots").
}
\usage{
residual.plots(lm.object, X=dft$x,
               layout=c(dim(X)[2],1),
               par.strip.text=list(cex=.8),
               scales.cex=.6,
               na.action=na.pass,
               y.relation="free",
               ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.object}{An object inheriting from \code{"lm"}.
    It may be necessary for the \code{lm.object} to be constructed with
    arguments \code{x=TRUE, y=TRUE}.}
  \item{X}{The x matrix of predictor variables used in the linear model
    \code{lm.object}.}
  \item{layout, par.strip.text}{trellis or lattice arguments.}
  \item{scales.cex}{\code{cex} argument forwarded to the scales argument
  of \code{xyplot}.}
  \item{na.action}{A function to filter missing data.  See \code{lm}.}
  \item{y.relation}{See \code{relation} in the discussion of the
    \code{scales} argument in
%% R
#ifndef S-Plus
    \code{\link[lattice]{xyplot}}.
#endif
#ifdef S-Plus
    \code{\link[trellis]{trellis.args}}.
#endif
  }
  \item{\dots}{Other arguments for \code{xysplom} or \code{xyplot}.}
}
\value{
  A list of four trellis objects, one for each of the four sets of
  plots.  The objects are named \code{"y.X"}, \code{"res.X"}
  \code{"pres.X"}, \code{"pres.Xj"}.  The default "printing" of the
  result will produce four pages of plots, one set per page.  They are
  often easier to read when all four sets appear as separate rows on one
  page (this usually requires an oversize device), or two rows are
  printed on each of two pages.
}
\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course with Examples in R}.
Second Edition.
Springer-Verlag, New York.
\url{https://link.springer.com/book/10.1007/978-1-4939-2122-5}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{residual.plots.lattice}}}
\examples{
if.R(s={
     longley <- data.frame(longley.x, Employed = longley.y)
     },r={
     data(longley)
     })

longley.lm <- lm( Employed ~ . , data=longley, x=TRUE, y=TRUE)
## 'x=TRUE, y=TRUE' are needed to pass the S-Plus CMD check.
## They may be needed if residual.plots() is inside a nested set of
## function calls.

tmp <- residual.plots(longley.lm)

## print two rows per page
print(tmp[[1]], position=c(0, 0.5, 1, 1.0), more=TRUE)
print(tmp[[2]], position=c(0, 0.0, 1, 0.5), more=FALSE)
print(tmp[[3]], position=c(0, 0.5, 1, 1.0), more=TRUE)
print(tmp[[4]], position=c(0, 0.0, 1, 0.5), more=FALSE)

## print as a single trellis object
ABCD <- do.call(rbind, lapply(tmp, as.vector))
dimnames(ABCD)[[1]] <- dimnames(tmp[[1]])[[1]]
ABCD
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{regression}
