\name{if.R}
\alias{if.R}
\alias{is.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Conditional Execution for R or S-Plus }
\description{
  \code{if.R} uses the \code{is.R} function to determine whether to
  execute the expression in the \code{r} argument or the expression
  in the \code{s} argument. \code{is.R}, copied from the now defunct
  base R function, returns \code{TRUE} if running under R and returns
  \code{FALSE} otherwise (initially designed for S/S-PLUS).
}
\usage{
if.R(r, s)

is.R()
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

  \code{is.R} is copied from the now defunct base R function of the same name.
}
\value{
  For \code{if.R} the result of the executed expression.

  \code{is.R} returns \code{TRUE} if we are using R and \code{FALSE} otherwise.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{R.Version}}
\examples{
 if.R(r={"This is R."},
      s={"This is S-Plus"})

 is.R()
}
\keyword{utilities}% at least one, from doc/KEYWORDS
