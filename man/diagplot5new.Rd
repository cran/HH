\name{diagplot5new}
\alias{diagplot5new}
\title{
Transpose of ECDF for centered fitted values and residuals from a linear model.
}
\description{
Transpose of ECDF (Empirical CDF) for centered fitted values and residuals from a linear model.
}
\usage{
diagplot5new(linearmodel, ..., pch = 19)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{linearmodel}{
\code{"lm"} object.
}
  \item{pch, \dots}{
Arguments to \code{\link[lattice]{xyplot}}.
}
}
\details{
  This is an implementation in \code{\link[lattice]{xyplot}}
of the "r-f spread" plot.
}
\value{
    \code{"trellis"} object.
}
\references{
William Cleveland (1993), \emph{Visualizing Data}, Hobart Press.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## See ?residVSfitted
\dontrun{
data(fat)
fat.lm <- lm(bodyfat ~ abdomin, data=fat)
diagplot5new(fat.lm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
