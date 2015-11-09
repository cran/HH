\name{lmplot}
\alias{lmplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Four types of residual plots for linear models.
}
\description{
Four types of residual plots for linear models.  The first three are redesigns of
plots that \code{stats:::plot.lm} presents.
The first two show the positive residuals in \code{col[2]} and the
negative residuals
in color \code{col[1]}.  The third and fourth use color \code{col[1]}.
The fourth is based on an S-Plus
panel that \R\ doesn't provide.
}
\usage{
lmplot(lm.object, ..., main=NULL,
       col=trellis.par.get("superpose.symbol")$col[1:2],
       ylim=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.object}{
Linear model object.  See \code{\link{lm}} for details.
}
\item{col}{Vector of color names.  Only the first two are used.
  If not specified, then
  \code{trellis.par.get("superpose.symbol")$col[1:2]} is used.
}
\item{main}{standard \code{main} title for plots.}
\item{ylim}{standard \pkg{lattice} argument.
It is used as specified for the \code{residVSfitted}, \code{diagQQ}, and
\code{diagplot5new} plots.  For the \code{scaleLocation} plot, the
\code{ylim} is modified to \code{c(0, max(abs(ylim)))}.  The main reason
for using the \code{ylim} argument is to allow visual comparison of
the residuals for two different models on the same scale.
}
\item{\dots}{
Other arguments, currently ignored.
}
}
\details{
The trellis plots from  the four functions \code{\link{residVSfitted}},
\code{\link{scaleLocation}}, \code{\link{diagQQ}}, \code{\link{diagplot5new}}
are displayed on the current device in a coordinated display.
}
\value{
A list of three trellis objects is returned invisibly, the first
contains the result of \code{\link{residVSfitted}} and
\code{\link{scaleLocation}} together.  The second \code{\link{diagQQ}},
and the third \code{\link{diagplot5new}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{residVSfitted}}, \code{\link{scaleLocation}},
  \code{\link{diagQQ}}, \code{\link{diagplot5new}}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
tmp <- data.frame(y=rnorm(100), x1=rnorm(100), x2=rnorm(100))
tmp.lm <- lm(y ~ x1 + x2, data=tmp)
lmplot(tmp.lm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
