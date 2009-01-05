\name{ci.plot}
\alias{ci.plot}
\alias{ci.plot.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot confidence and prediction intervals for simple linear regression}
\description{
The data, the least squares line, the confidence interval lines, and the
prediction interval lines for a simple
linear regression (\code{lm(y ~ x)}) are displayed.  Tick marks are
placed at the location of xbar, the x-value of the narrowest interval.
}
\usage{
ci.plot(lm.object, ...)

\method{ci.plot}{lm}(lm.object,
        xlim=range(data[, x.name]),
        newdata,
        conf.level=.95,
        data=model.frame(lm.object),
        newfit,
        ylim,
        pch=16,
        main.cex=1,
        main=list(paste(100*conf.level,
          "\% confidence and prediction intervals for ",
          substitute(lm.object), sep=""), cex=main.cex), ...
        )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.object}{Linear model for one \code{y} and one \code{x} variable.}
  \item{xlim}{\code{xlim} for plot.  Default is based on data from which
        \code{lm.object} was constructed.}
  \item{newdata}{\code{data.frame} containing data for which predictions
    are wanted.  The variable name of the column must be identical to
    the name of the predictor variable in the model object.
    Defaults to a data.frame containing a vector
    spanning the range of observed data.  User-specified values are
    appended to the default vector.}
  \item{conf.level}{Confidence level for intervals, defaults to \code{.95}}
  \item{data}{\code{data} extracted from the \code{lm.object}}
  \item{newfit}{Constructed \code{data.frame} containing the
    predictions,confidence interval, and prediction interval
    for the \code{newdata}.}
  \item{ylim}{\code{ylim} for plot.  Default is based on the 
        constructed prediction interval.}
  \item{pch}{Plotting character for observed points.}
  \item{main.cex}{Font size for main title.}
  \item{main}{Main title for plot}
  \item{\dots}{Additional arguments to be passed to panel function.}
}
\value{
\code{"trellis"} object containing the plot.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
  The \code{predict.lm} functions in S-Plus and R differ.
  The S-Plus function can produce both confidence and prediction
  intervals with a single call.  The R function produces only one
  of them in a single call.  Therefore the default calculation of
  \code{newfit} within the function depends on the system.
}
\seealso{\code{\link[stats]{lm}}, \code{\link[stats]{predict.lm}}}
\examples{
tmp <- data.frame(x=rnorm(20), y=rnorm(20))
tmp.lm <- lm(y ~ x, data=tmp)
ci.plot(tmp.lm)
}
\keyword{ hplot }
\keyword{ regression }
