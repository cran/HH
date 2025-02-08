\name{lmatPairwise}
\alias{lmatPairwise}
\alias{lmatPairwise.matrix}
\alias{lmatPairwise.glht}
\alias{lmatPairwise.mmc.multicomp}
\alias{lmatPairwise.mmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
lmatPairwise
}
\description{
lmatPairwise
}
\usage{
lmatPairwise(x, ...)
\method{lmatPairwise}{matrix}(x, levels, ...)
\method{lmatPairwise}{glht}(x, ...)
\method{lmatPairwise}{mmc.multicomp}(x, ...)
\method{lmatPairwise}{mmc}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x
}
  \item{\dots}{
\dots
}
  \item{levels}{
levels
}
}
\details{
details
}
\value{
matrix
}
\author{
rmh
}
\seealso{
\code{\link{mmc}}, \code{\link[multcomp]{mcp}}
}
\examples{
data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
catalystm.mmc <- mmc(catalystm1.aov)
lmatPairwise(catalystm.mmc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ design }% __ONLY ONE__ keyword per line
