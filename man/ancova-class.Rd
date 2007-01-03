\name{ancova-class}
\docType{class}
\alias{ancova-class}

\title{Class "ancova" Analysis of Covariance }
\description{Analysis of Covariance.  The class is an extension of "aov"
and "lm".  It is identical to the "aov" for a single factor and a single
covariate plus an attribute which contains a \code{"trellis"} object.
Four different models are included in the class.  See \code{\link{ancova}}
for the examples.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Extends}{
Class \code{"aov"}, directly.
Class \code{"lm"}, by class "aov", distance 2.
Class \code{"mlm"}, by class "aov", distance 2, with explicit test and coerce.
Class \code{"oldClass"}, by class "aov", distance 3.
Class \code{"oldClass"}, by class "aov", distance 4, with explicit test and coerce.
}
\section{Methods}{
  No methods defined with class "ancova" in the signature.
  S3-type methods are
  \code{"anova.ancova"}, \code{"coef.ancova"},
  \code{"coefficients.ancova"}, \code{"model.frame.ancova"},
  \code{"plot.ancova"}, \code{"predict.ancova"},
  \code{"print.ancova"}, \code{"summary.ancova"}.
  \code{"plot.ancova(x)"} plots a standard lm plot of \code{x}.
  \code{"print.ancova(x)"} prints the \code{anova(x)} and the trellis attribute.
  The remaining methods use \code{NextMethod}.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{ancova}}
}
\keyword{classes}
