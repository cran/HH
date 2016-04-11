\name{objip}
\alias{objip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{loop through all attached directories looking for pattern,
  possibly restricting to specified class or mode.}
\description{
  Loop \code{objects()} through all attached directories
  (items in the \code{search()} list) looking for a regular expression pattern.
}
\usage{
objip(pattern, where = search(), all.names=FALSE, mode="any", class,
      ls.function=if (mode != "any" || !missing(class)) "ls.str" else "ls")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{Character string containing a regular expression
    that is used to list only a subset of the objects.
    Only names matching 'pattern' are returned.}
  \item{where}{an object defining a database in the search list.}
  \item{all.names}{In R, a logical that is passed to the \code{ls} function.
    If 'TRUE', all object names are returned.
    If 'FALSE', names which begin with a '.' are omitted.}
  \item{mode, class}{See \code{\link[utils]{ls.str}} and
    \code{\link[base]{mode}} for storage mode of an object.
    See \code{\link[base]{class}} for object classes.}
  \item{ls.function}{Either \code{ls} or \code{ls.str}.  If either
    \code{mode} or \code{class} is specified then the default is
    \code{ls.str}.  If neither is specified then the default is
    \code{ls}.}
}
\value{
A list of 0 or more character vectors.  Each character vector has the
name of one of the items in the \code{search()} list.  Each character
vector contains the names of the objects in the specified environment
which match the \code{pattern}.  If there are no matching names in an
environment, then the corresponding character vector is removed from the
result.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link[base]{ls}},
}
\examples{
objip("qq")
objip("^qq")
objip("qq$")
\dontrun{
## R only
objip("rowSums", all.names=TRUE)

## list all objects in the second and third attached packages
search()
objip()[2:3]
}
}
\keyword{utilities}
