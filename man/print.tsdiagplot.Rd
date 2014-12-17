\name{print.tsdiagplot}
\alias{print.tsdiagplot}
\alias{print1.tsdiagplot}
\alias{print2.tsdiagplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print a "tsdiagplot" object.}
\description{
  Print a \code{"tsdiagplot"} object.
}
\usage{
\method{print}{tsdiagplot}(x, ..., portrait=FALSE)
print1.tsdiagplot(x)
print2.tsdiagplot(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{"tsdiagplot"} object}
  \item{\dots}{Optional arguments to \code{print}.
    The only \dots\ argument that is used is \code{pages}.
    If \code{pages} is not used or \code{pages==1}, then use \code{print1.tsdiagplot}.
    If \code{pages!=1}, then use \code{print2.tsdiagplot}.
  }
  \item{portrait}{logical.  If \code{FALSE}, arrange the panels for a
    landscape orientation (pdf with width=12 inches looks good).
    If \code{TRUE}, arrange the panels for a
    portrait orientation (pdf with height=13 inches looks good).}
}
\details{
  A \code{"tsdiagplot"} object is a collection of several
  \code{"trellis"} objects.  We provide two options for printing them.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{tsdiagplot}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
