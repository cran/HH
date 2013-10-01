\name{objip}
\alias{objip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{loop through all attached directories looking for pattern}
\description{
  Loop \code{objects()} through all attached directories
  (items in the \code{search()} list) looking for a regular expression pattern.
}
\usage{
objip(pattern, where = search(), frame=NULL, all.names=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{Character string containing a regular expression
    that is used to list only a subset of the objects.
    Only names matching 'pattern' are returned.}
  \item{where}{an object defining a database in the search list.}
  \item{frame}{In S-Plus, an integer giving the frame number.
    In R, \code{frame} is ignored.}
  \item{all.names}{In R, a logical that is passed to the \code{ls} function.
    If 'TRUE', all object names are returned.
    If 'FALSE', names which begin with a '.' are omitted.}
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
%% R
#ifndef S-Plus
  \code{\link[base]{ls}},
#endif
#ifdef S-Plus
  \code{\link[splus]{objects}}.
#endif
}
\examples{
objip("qq")
objip("^qq")
objip("qq$")
\dontrun{
## R only
objip("rowSums", all.names=TRUE)
}
}
\keyword{utilities}
