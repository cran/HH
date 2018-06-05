\name{rbind.trellis}
\alias{rbind.trellis}
\alias{transpose.trellis}
\alias{transpose}
\alias{transpose.default}
\alias{aperm.trellis}
\alias{cbind.trellis}
\title{
Extend matrix reshaping functions to trellis objects.
}
\description{
Extend matrix reshaping functions to trellis objects.
See the details section for comparisons with similar functions in the
\pkg{lattice} package.
}
\usage{
transpose(x)
\method{transpose}{trellis}(x)
\method{transpose}{default}(x)
\method{aperm}{trellis}(a, perm, ...)
\method{rbind}{trellis}(..., deparse.level=1,
      combineLimits=TRUE, useOuterStrips=TRUE)
\method{cbind}{trellis}(..., deparse.level=1,
      combineLimits=TRUE, useOuterStrips=TRUE)
}
\arguments{
  \item{\dots, x, a}{
    A set of \code{trellis} objects.
  }
  \item{perm}{
    Permutation vector, see \code{\link{aperm}} for details.
  }
\item{combineLimits, useOuterStrips}{logical.  If \code{TRUE} (the
  default), use the similarly named \pkg{latticeExtra} functions before
  returning the result.
}
\item{deparse.level}{
See \code{\link{cbind}} for details.  These functions ignore this
argument and always use the \code{names(list(...))}, if non-\code{NULL},
for the labels.
If \code{NULL}, then the first \code{length(list(...))} uppercase
letters are used.
}
}
\value{
trellis object constructed from arguments with new dim and layout.
}
\details{
  \code{transpose.trellis} tries to capture and modify all potentially relevant
  trellis components.  \code{transpose.trellis} is more comprehensive
  than the similar
  \code{\link[lattice:update.trellis]{t.trellis}} which adjusts only the
    \code{perm.cond} component.

  \code{aperm.trellis} does not attempt to check all potentially relevant
  trellis components.
  It does not adjust \code{layout.heights}, \code{layout.widths}, or
  \code{between}
  It may show strange axis
  positions or strip positions for any non-standard arrangement, for example,
  for any trellis object that has already been
  through \code{latticeExtra::combineLimits}.
  }
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
F <- xyplot((1:15) ~ (1:15) | rep(factor(letters[3:5]), each=5))
G <- xyplot((1:18) ~ (1:18) | rep(factor(letters[3:5]), each=6))
rbind(AAA=F, BBB=G)
cbind(AAA=F, BBB=G)

tmp <- data.frame(y=1:24,
                  x=1:24,
                  a=rep(letters[1:2], each=12),
                  b=rep(letters[3:5], each=4, times=2),
                  c=rep(letters[6:9], times=6))

t3 <- xyplot(y ~ x | c*b*a, data=tmp,
             panel=function(x, y, ...) panel.text(x, y, y),
             scales=list(alternating=FALSE))
## t3
t3u <- update(t3, layout=c(4*3, 2), between=list(x=c(0,0,0,1)), main="t3")
useOuterStripsT2L1(t3u)

\dontrun{
## update(t3, layout=c(24, 1))

t3.321 <- aperm(t3, c(3,2,1))
update(t3.321, main="t3.321", layout=c(6,4), between=list(x=c(0,1))) ## 2*3,4

try(transpose(t3))   ## requires a one- or two-dimensional trellis object.

t3.123 <- aperm(t3, c(1,2,3)) ## identity operation
t3.132 <- aperm(t3, c(1,3,2))
t3.213 <- aperm(t3, c(2,1,3))
t3.231 <- aperm(t3, c(2,3,1))
t3.312 <- aperm(t3, c(3,1,2))
t3.321 <- aperm(t3, c(3,2,1))

u3.123 <- update(t3.123, main="t3.123", layout=c(12,2),
                 between=list(x=c(0,0,0,1))) ## 4*3,2
u3.132 <- update(t3.132, main="t3.132", layout=c(8,3),
                 between=list(x=c(0,0,0,1))) ## 4*2,3
u3.213 <- update(t3.213, main="t3.213", layout=c(3,8),
                 between=list(y=c(0,0,0,1)), par.strip.text=list(cex=.8)) ## 3,4*2
u3.231 <- update(t3.231, main="t3.231", layout=c(6,4),
                 between=list(x=c(0,0,1))) ## 2*3,4
u3.312 <- update(t3.312, main="t3.312", layout=c(2,12),
                 between=list(y=c(0,0,0,1)), par.strip.text=list(cex=.6)) ## 2,3*4
u3.321 <- update(t3.321, main="t3.321", layout=c(6,4),
                 between=list(x=c(0,1))) ## 2*3,4

pdf("u5.pdf", width=17, height=22)
print(u3.123, split=c(1,1,2,3), more=TRUE)
print(u3.132, split=c(2,1,2,3), more=TRUE)
print(u3.213, split=c(1,2,2,3), more=TRUE)
print(u3.231, split=c(2,2,2,3), more=TRUE)
print(u3.312, split=c(1,3,2,3), more=TRUE)
print(u3.321, split=c(2,3,2,3), more=FALSE)
dev.off()

try(transpose(t3.123)) ## layout is a matrix, but dim is not.
}

\dontrun{
t2 <- xyplot(y ~ x | b*c, data=tmp,
             panel=function(x, y, ...) panel.text(x, y, y),
             scales=list(alternating=FALSE))
t2

## aperm(t2, 1:2) ## identity

transpose(t2)
aperm(t2, 2:1)

t1a <- xyplot(y ~ x | b, data=tmp[tmp$a=="a",])
t1b <- xyplot(y ~ x | b, data=tmp[tmp$a=="b",])
t1a
t1b

rbind(t1a, t1b)
rbind(AAA=t1a, BBB=t1b)

cbind(t1a, t1b)
cbind(AAA=t1a, BBB=t1b)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
