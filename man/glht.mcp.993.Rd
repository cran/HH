\name{glht.mcp.993}
\alias{glht.mcp.993}
\alias{mcp2matrix.993}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retain averaging behavior that was previously available in glht.}
\description{
  For some ANOVA models with two or more factors, we need to average over
  interaction terms.  These functions use an older version of glht.mcp
  and mcp2matrix to do that averaging.
}
\usage{
glht.mcp.993(model, linfct, ...)
mcp2matrix.993(model, linfct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model, linfct, \dots}{See \code{\link[multcomp]{glht}}}
}
\details{
\code{mcp2matrix} is taken from from \code{multcomp_0.993-2.tar.gz/R/mcp.R}.
\code{glht.mcp.993} is based on \code{glht.mcp} in
\code{multcomp_1.0-0/R/glht.R}
with the call to \code{mcp2matrix} replaced by a call to \code{mcp2matrix.993}.
}
\value{
See \code{\link[multcomp]{glht}}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[HH]{MMC}}}
\keyword{ htest }
\keyword{design }
