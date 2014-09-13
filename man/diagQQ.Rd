\name{diagQQ}
\alias{diagQQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QQ plot of regression residuals.
}
\description{
QQ plot of regression residuals. The \code{\link[lattice]{panel.qqmathline}} is displayed.
}
\usage{
diagQQ(lm.object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.object}{
\code{"lm"} object.
}
  \item{\dots}{
Additional arguments to \code{qqmath}.
}
}
\value{
  \code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[lattice]{qqmath}}
}
\examples{
## See ?residVSfitted
\dontrun{
data(fat)
fat.lm <- lm(bodyfat ~ abdomin, data=fat)
diagQQ(fat.lm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
