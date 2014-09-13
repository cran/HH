\name{useOuterStripsT2L1}
\alias{useOuterStripsT2L1}
\title{Three-factor generalization of latticeExtra::useOuterStrips}
\description{Three-factor generalization of \code{latticeExtra::useOuterStrips}}
\usage{
useOuterStripsT2L1(x, ..., strip.height=.4, strip.names=c(TRUE, TRUE))
}
\arguments{
  \item{x}{A \code{lattice} object with \code{dim(x)==3}.}
  \item{\dots}{Additional arguments to be forwarded to the
    \code{\link[lattice]{strip.default}} function.}
  \item{strip.height}{Height of each the strip for each factor.  The
    number of factors in the top and left strips may not be the same.
    This argument is multiplied by the number of factors in each
    location
    and sent on to the \pkg{lattice} \code{par.settings} argument for the
  \code{layout.widths$strip.left} and \code{layout.heights$strip}
  components.
}
  \item{strip.names}{See \code{\link[lattice]{strip.default}}.}
}
\value{
A \code{trellis} object with two factors in the top \code{strip} and 1
factor in the \code{strip.left}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
tmp <- data.frame(A=rep(factor(letters[1:2]), each=12),
                  B=rep(factor(letters[3:5]), each=4, times=2),
                  C=rep(factor(letters[6:9]), times=6),
                  x=1:24,
                  y=1:24)

F <- xyplot(y ~ x | B*A*C, data=tmp,
            panel=function(x, y, labels, ...) {
              panel.text(x, y, matrix(1:24, 6, 4, byrow=TRUE)[panel.number()], ...)
            },
            layout=c(6, 4), between=list(x=c(.5, .5, 1.5), y=1))
F

useOuterStripsT2L1(F)

}
\keyword{ dplot }
