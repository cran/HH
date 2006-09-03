\name{if.R}
\alias{if.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Conditional Execution for R or S-Plus }
\description{
  \code{if.R} uses the \code{is.R} function to determine whether to
  execute the expression in the \code{r} argument or the expression
  in the \code{s} argument.
}
\usage{
if.R(r, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{Any R expression, including a group of expressions nested in
    braces.  Assignments made in this expression are available
    to the enclosing function.}
  \item{s}{Any S-Plus expression, including a group of expressions
    nested in braces.  Assignments made in this expression are available
    to the enclosing function.}
}
\details{
  Not all functions are in both implementations of the S language.  In
  particular, panel functions for \code{lattice} in R (based on
  \code{grid} graphics) are very different from panel functions for
  \code{trellis} (based on the older graphics technology) in S-Plus.
}
\value{
  The result of the executed expression.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[base]{is.R}}}
\examples{
 if.R(r={"This is R."},
      s={"This is S-Plus"})
}
\keyword{utilities}% at least one, from doc/KEYWORDS
