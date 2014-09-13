\name{anova.ancovaplot}
\alias{anova.ancovaplot}
\alias{aov.ancovaplot}
\alias{aovStatement}
\alias{aovStatement.ancovaplot}
\alias{aovStatementAndAnova}
\alias{aovStatementAndAnova.ancovaplot}
\alias{model.tables.ancovaplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA table for a c("ancovaplot","trellis") object.
}
\description{
  ANOVA table for a c("ancovaplot","trellis") object.
}
\usage{
\method{anova}{ancovaplot}(object, ...)
aov.ancovaplot(object, warn=TRUE)
aovStatement(object, ...)
\method{aovStatement}{ancovaplot}(object, ...)
aovStatementAndAnova(object, ...)
\method{aovStatementAndAnova}{ancovaplot}(object, ...)
\method{model.tables}{ancovaplot}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object, x}{
\code{c("ancovaplot","trellis")} object.
}
\item{warn, \dots}{\code{warn} is logical with default \code{TRUE}.
  See the Details section for the interpretation of \code{warn}.  When \code{\dots} is
  received by \code{aov.ancovaplot}, it is evaluated if it is \code{warn} and
  ignored for all other values.  When \code{\dots} is
  received by \code{model.tables} it is interpreted normally.
}
}
\details{
  The \code{aov.ancovaplot} modifies the \code{call} item into an
  \code{aov} call with the same formula and data.  If there are
  \code{groups} in the call specified as a name, the \code{groups}
  factor is included in the constructed \code{aov} call only if there
  are both a factor and a covariate in the right-hand-side of the
  formula.  In that case they the \code{groups} will be interpreted as a
  block factor and will be placed first.  If the \code{groups} are
  specified as a vector of values in the call, the \code{groups} are
  ignored with a warning.  If there is only one term in the
  right-hand-side, then the \code{groups} factor will not be placed
  into the \code{aov} formula.  In this case, there will be a warning if
  the argument \code{warn} is \code{TRUE}, and no warning if the \code{warn}
  argument is \code{FALSE}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{ancovaplot}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
