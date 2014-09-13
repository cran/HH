\name{regrresidplot}
\alias{regrresidplot}
\alias{panel.residSquare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a plot of y vs x from a linear model object, with residuals
indicated by lines or squares.
}
\description{
Draw a plot of response vector y vs predictor variable x from a linear
model object all of whose predictors are a function of x, with residuals
indicated by lines or squares.
}
\usage{
regrresidplot(x, y, resid.plot = FALSE, fit.line=TRUE,
              lm.object = lm(y ~ x), x.name = names(lm.object$model)[2],
              col = trellis.par.get()$plot.symbol$col,
              col.yhat = NULL, col.fit = "gray80", col.resid = "gray40", ...)

panel.residSquare(x, y, yhat, resid.plot = FALSE, col = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Predictor variable.  Must be a vector or a single column.
}
  \item{y}{
Response variable.  Must be a vector or a single column.
}
  \item{yhat}{
Predicted value of y based on the model in \code{lm.object} over the
\code{xlim} range of the plot.
}
  \item{resid.plot}{
    Logical or character.  Should the residuals from \code{lm.object} be
    plotted, and how?
    Default is \code{FALSE}.  Alternatives are \code{TRUE} for lines and
    \code{"square"} for squares.
}
  \item{fit.line}{
    Logical.  Should the fitted regression line from \code{lm.object} be plotted?
    Default \code{TRUE}.
}
  \item{lm.object}{
Linear model object of y against some function of x.  The default value
is the simple linear regression of \code{lm(y ~ x)}.
}
  \item{x.name}{
Name of $x$-variable to be used in the construction of the fitted values.
}
  \item{col}{
Color of observed points.
}
  \item{col.yhat}{
Color of fitted points.  Default is \code{NULL}.
}
  \item{col.fit}{
Color of fitted line.
}
  \item{col.resid}{
    Color of residuals, either lines or squares depending on the value of
    \code{resid.plot}.
}
  \item{\dots}{
Additional arguments to the panel functions.
}
}
\value{
\code{regrresidplot} returns a \code{"trellis"} object.
\code{panel.residSquare} is a panel function with no useful returned value.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
data(fat)
fat.lm <- lm(bodyfat ~ abdomin, data=fat)

AA <- regrresidplot(fat$abdomin, fat$bodyfat, xlim=c(70,185), ylim=c(0,50))
BB <- regrresidplot(fat$abdomin, fat$bodyfat, xlim=c(70,185), ylim=c(0,50),
                    resid.plot="line")
CC <- regrresidplot(fat$abdomin, fat$bodyfat, xlim=c(70,185), ylim=c(0,50),
                    resid.plot="square")

update(between=list(y=1),
       c("Residuals Not Displayed"=AA,
         "Residual Lines"=BB,
         "Residual Squares"=CC, layout=c(1,3)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
