\name{perspPlane}
\alias{perspPlane}
%% \alias{persp.plane} %% old name
\alias{perspFloor}
\alias{perspBack.wall.x}
\alias{perspBack.wall.y}
% \alias{persp.setup} %% splus only
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Helper functions for regr2.plot}
\description{
Helper functions for \code{regr2.plot}.
}
\usage{
perspPlane(x, y, z, persp.out, ...)
perspFloor(x, y, z, persp.out, ...)
perspBack.wall.x(x, y, z, persp.out, ...)
perspBack.wall.y(x, y, z, persp.out, ...)
% persp.setup(...) ## persp.setup is not needed or used in R.
%                  ## persp.setup is in the "splus" Chapter in S-Plus.
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y,z}{Arguments to \code{trans3d} in R, or \code{perspp} in S-Plus.}
  \item{persp.out}{Result from previous call to \code{persp}.}
  \item{\dots}{Additional arguments to \code{persp}.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{regr2.plot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
