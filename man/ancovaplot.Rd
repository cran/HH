\name{ancovaplot}
\alias{ancovaplot}
\alias{ancovaplot.formula}
\alias{panel.ancova.superpose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analysis of Covariance Plots
}
\description{
  Analysis of Covariance Plots. Any of the ancova models\cr
  \code{y ~ x * t}\cr
  \code{y ~ t * x}\cr
  \code{y ~ x + t}\cr
  \code{y ~ t + x}\cr
  \code{y ~ x    , groups=t}\cr
  \code{y ~     t, x=x}\cr
  \code{y ~ x * t, groups=b}\cr
  \code{y ~ t * x, groups=b}\cr
  \code{y ~ x + t, groups=b}\cr
  \code{y ~ t + x, groups=b}
}
\usage{
ancovaplot(object, ...)
\method{ancovaplot}{formula}(object, data, groups=NULL, x=NULL, ...,
           formula=object,
           col=rep(tpg$col,
             length=length(levels(as.factor(groups)))),
           pch=rep(c(15,19,17,18,16,20, 0:14),
             length=length(levels(as.factor(groups)))),
           slope, intercept,
           layout=c(length(levels(cc)), 1),
           col.line=col, lty=1,
           superpose.panel=TRUE,
           between=if (superpose.panel)
                      list(x=c(rep(0, length(levels(cc))-1), 1))
                   else
                      list(x=0),
           col.by.groups=FALSE ## ignored unless groups= is specified
           )

panel.ancova.superpose(x, y, subscripts, groups,
                       slope, intercept,
                       col, pch, ...,
                       col.line, lty,
                       superpose.panel,
                       col.by.groups,
                       condition.factor,
                       groups.cc.incompatible,
                       plot.resids=FALSE,
                       print.resids=FALSE,
                       mean.x.line=FALSE,
                       col.mean.x.line="gray80")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula, object}{
\code{formula} specifying the \code{aov} model.  The function modifies it for the
\code{xyplot} specification.
}
  \item{data}{
\code{data.frame}
}
  \item{groups}{
    If the treatment factor is included in the \code{formula}, then \code{groups} is not
    needed.  By default \code{groups} will be set to the treatment factor, but the
    user may specify another factor for \code{groups}, usually a blocking factor.  The
    \code{pch} will follow the value of \code{groups}.  If the treatment
    is not included in the \code{formula}, then \code{groups} is required.
  }
  \item{x}{
Covariate.  Required by \code{ancovaplot.formula} if the covariate is
not included in the \code{formula}.

For \code{panel.ancova.superpose}, see \code{\link{panel.superpose}}.
}
  \item{\dots}{
Other arguments to be passed to \code{xyplot}.
}
  \item{col, pch}{
Standard \pkg{lattice} arguments.  \code{pch} follows the value of
\code{groups}.  When \code{col.by.groups} is \code{TRUE}, then
\code{col} follow the value of \code{groups}.
When \code{col.by.groups} is \code{FALSE}, then
\code{col} follows the value of the treatment factor, and is constant in
each panel.
}
  \item{slope, intercept}{
Vector, the length of the number of treatment levels, containing slope
and intercept of the \code{abline} in each panel.
This is by default calculated based on the formula.  The user may
override each independently.
}
  \item{layout, between}{
Standard \pkg{lattice} arguments.
}
  \item{col.line, lty}{
Standard \pkg{lattice} arguments.  They follow the value of the
treatment factor in the \code{formula}.
}
  \item{y, subscripts}{
See \code{\link{panel.xyplot}}.
}
  \item{superpose.panel}{
logical.  if \code{TRUE} (the default), there is an additional panel on
the right containing the superposition of the points and lines for all treatment levels.
}
  \item{col.by.groups}{
logical. See the discussion in argument \code{col}.
}
\item{condition.factor, groups.cc.incompatible}{
  These are both internal variables.  \code{condition.factor} contains a
  copy of the treatment factor.  \code{groups.cc.incompatible} is a
  logical which is set to \code{TRUE} when the \code{groups} argument is
  explicitly set by the user.
}
\item{plot.resids, print.resids, mean.x.line, col.mean.x.line}{
  logical, logical, logical or numeric, color name.
  When \code{plot.resids==TRUE} then vertical line segments
  connecting the data points and the fitted line are drawn.
  The other two arguments are interpreted only when
  \code{plot.resids==TRUE}.
  When \code{print.resids==TRUE} then the values of the residuals are
  printed on the console.  When \code{is.numeric(mean.x.line)} then a vertical
  reference line is drawn at the specified value, which will normally be
  specified by the user as the mean of the full set of x values.
  The reference line will have color specified by \code{col.mean.x.line}.
}
}
\details{
  \tabular{llll}{
  \code{ancova=aov specification}\tab\code{xyplot specification}\tab\code{abline         }\tab\cr
  \code{y ~ x * t               }\tab\code{y ~ x | t, groups=t }\tab\code{lm(y[t] ~ x[t])}\tab\code{## separate lines}\cr
  \code{y ~ t * x               }\tab\code{y ~ x | t, groups=t }\tab\code{lm(y[t] ~ x[t])}\tab\code{## separate lines}\cr
  \code{y ~ x + t               }\tab\code{y ~ x | t, groups=t }\tab\code{lm(y ~ x + t)  }\tab\code{## parallel lines}\cr
  \code{y ~ t + x               }\tab\code{y ~ x | t, groups=t }\tab\code{lm(y ~ x + t)  }\tab\code{## parallel lines}\cr
  \code{y ~ x    , groups=t     }\tab\code{y ~ x | t, groups=t }\tab\code{lm(y ~ x)      }\tab\code{## single regression line}\cr
  \code{y ~     t, x=x          }\tab\code{y ~ x | t, groups=t }\tab\code{mean(t)        }\tab\code{## separate horizontal lines}\cr
  \code{y ~ x * t, groups=b     }\tab\code{y ~ x | t, groups=b }\tab\code{lm(y[t] ~ x[t])}\tab\code{## sep lines, pch&col follow b}\cr
  \code{y ~ t * x, groups=b     }\tab\code{y ~ x | t, groups=b }\tab\code{lm(y[t] ~ x[t])}\tab\code{## sep lines, pch&col follow b}\cr
  \code{y ~ x + t, groups=b     }\tab\code{y ~ x | t, groups=b }\tab\code{lm(y ~ x + t)  }\tab\code{## par lines, pch&col follow b}\cr
  \code{y ~ t + x, groups=b     }\tab\code{y ~ x | t, groups=b }\tab\code{lm(y ~ x + t)  }\tab\code{## par lines, pch&col follow b}
}
}
\value{
  \code{ancovaplot} returns a \code{c("ancova","trellis")} object.
  \code{panel.ancova.superpose} is an ordinary \pkg{lattice} \code{panel} function.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\references{ Heiberger, Richard M. and Holland, Burt (2004).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\seealso{
  See the older \code{\link{ancova}}.
  }
\examples{
  data(hotdog, package="HH")
  ancovaplot(Sodium ~ Calories + Type, data=hotdog)
  ancovaplot(Sodium ~ Calories * Type, data=hotdog)
  ancovaplot(Sodium ~ Calories, groups=Type, data=hotdog)
  ancovaplot(Sodium ~ Type, x=Calories, data=hotdog)

  ## Please see demo("ancova", package="HH") to coordinate placement
  ## of all four of these plots on the same page.

  ancovaplot(Sodium ~ Calories + Type, data=hotdog, plot.resids=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{dplot}
\keyword{models}
\keyword{regression}
