\name{positioned-class}
\docType{class}
\alias{positioned-class}

\title{Class "positioned", extends "ordered" to specify the position
  for graphing the levels of a factor.}
\description{The default values for plotting a factor \code{x} are the integers
  \code{1:length(levels(x))}.  This class and its functions provide a way of
  specifying alternate plotting locations for the levels.
}
\section{Objects from the Class}{A virtual Class:
  No objects may be created from it.}
\section{Extends}{
Class \code{"ordered"}, directly.
Class \code{"factor"}, by class "ordered", distance 2.
Class \code{"oldClass"}, by class "ordered", distance 3.
}
\section{Methods}{
No methods defined with class "positioned" in the signature.
  S3-type methods are
\code{"[.positioned"},
\code{as.double.positioned},
\code{as.numeric.positioned},
\code{as.positioned},
\code{is.numeric.positioned},
\code{is.positioned},
\code{positioned},
\code{print.positioned},
\code{unique.positioned}.\cr
Although \code{interaction.positioned} should be a method, it isn't
because \code{interaction} is not a generic and can't easily be made one
since the name \code{interaction.plot} conflicts.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{position}}.}
\keyword{classes}
