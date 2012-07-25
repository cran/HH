\name{diag.maybe.null}
\alias{diag.maybe.null}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Returns a value for the diagonal of NA and NULL arguments.}
\description{
  Returns the argument for the diagonal of NA and NULL arguments.
  For all other arguments, it calls the regular \code{diag} function.
}
\usage{
diag.maybe.null(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix, vector, NA, }
  \item{\dots}{Other arguments to \code{diag}.}
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{
%% R
#ifndef S-Plus
  \code{\link[base]{diag}}.
#endif
#ifdef S-Plus  
  \code{\link[splus]{diag}}.
#endif
}
\examples{
diag.maybe.null(NULL)
diag.maybe.null(NA)
diag.maybe.null(1:5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NA}
