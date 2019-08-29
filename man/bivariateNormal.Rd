\name{bivariateNormal}
\alias{bivariateNormal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the bivariate normal density using wireframe for specified rho.
}
\description{
  Plot the bivariate normal density using wireframe for specified rho.
  There is a shiny app that allows this to be done dynamically.
}
\usage{
bivariateNormal(rho = 0, layout = c(3, 3), lwd = 0.2,
                angle = c(22.5, 67.5, 112.5, 337.5, 157.5, 292.5, 247.5, 202.5),
                col.regions = trellis.par.get("regions")$col, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{
Correlation between $x$ and $y$.
}
  \item{layout, lwd}{
Standard \pkg{lattice} arguments.
}
  \item{angle}{
    This is used as the \code{z} component of the \code{screen} argument to
    \code{\link[lattice:panel.cloud]{panel.wireframe}}.
}
  \item{col.regions, \dots}{
See \code{\link[lattice:cloud]{wireframe}}.
}
}
\details{
  The default setting shows the view as seen from a series of eight angles.
  To see just a single view, see the example.
}
\value{
\code{"trellis"} object.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\note{
  Based on the \code{galaxy} example on pages 204--205 in
  \emph{S \& S-PLUS Trellis Graphics User's Manual},
  Richard A. Becker and William S. Cleveland (1996),
\url{https://www.stat.auckland.ac.nz/~ihaka/courses/120/trellis.user.pdf} %% works 19Aug2019
% \url{http://www.albany.edu/acc/courses/acc522fall2007/lecturenotes/trellis.usermanual.pdf}.
}
\examples{
bv8 <- bivariateNormal(.7)  ## all views on one page
bv8
update(bv8[3], layout=c(1,1)) ## one panel
\dontrun{
  shiny::runApp(file.path(system.file(package="HH"), "shiny/bivariateNormal")) ## 3D
  shiny::runApp(system.file("shiny/bivariateNormalScatterplot", package="HH")) ## scatterplota
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{shiny}
