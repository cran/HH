\name{hovBF}
\alias{hovBF}
\alias{hovplotBF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Homogeneity of Variance:  Brown--Forsyth method
}
\description{
Homogeneity of Variance:  Brown--Forsyth method
}
\usage{
hovBF(x, data = sys.parent(), ..., na.rm = TRUE)
hovplotBF(x, data, ..., na.rm = TRUE,
          main = "Brown-Forsyth Homogeneity of Variance", plotmath = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Model formula with one response variable and one factor.
}
  \item{data}{
\code{data.frame}
}
  \item{\dots}{
Other arguments. \code{hovplotBF} sends them on to the panel function. \code{hovBF} ignores them.
}
\item{na.rm}{A logical value indicating whether 'NA' values should be
          stripped before the computation proceeds. See \code{\link[stats]{median}}.}
  \item{main}{
\code{main} title for the plot.
}
  \item{plotmath}{
Logical.  When \code{TRUE} (the default) the strip labels use
\code{plotmath}. When \code{FALSE} the strip labels use ASCII.
}
}
\value{
  \code{hovplotBF} returns a three-panel \code{trellis} object.
  \code{hovBF} returns an \code{htest} object.
}
\references{
Brown, M.~B. and Forsyth, A.~B. (1974).
``Robust tests for equality of variances.''
\emph{Journal of the American Statistical Association}, 69:364--367.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(batch)
batch1.aov <- aov(Calcium ~ Batch, data=batch)
anova(batch1.aov)
hovBF(Calcium ~ Batch, data=batch)
hovplotBF(Calcium ~ Batch, data=batch)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
