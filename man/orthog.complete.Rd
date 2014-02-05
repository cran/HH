\name{orthog.complete}
\alias{orthog.complete}
\alias{orthog.construct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construct an orthogonal matrix which is an arbitrary completion
  of the column space of the input set of columns.}
\description{
Construct an orthogonal matrix which is an arbitrary completion
  of the column space of the input set of columns.
}

\usage{
orthog.complete(x, normalize=TRUE, abs2.rows=1:nrow(x),
                Int=TRUE, drop.Int=Int)

orthog.construct(y, x, x.rows, normalize=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{For \code{orthog.complete}, an n-row matrix of one or more
    columns.\cr
    For \code{orthog.construct}, a set of contrasts for a factor.}
  \item{y}{matrix of coefficients specifying the linear combinations
    estimated.  This will usually be the \code{lmat} from an S-Plus
    \code{"multicomp"} object or the \code{linfct} component from an
    R \code{"glht"} object.}

  \item{normalize, abs2.rows, x.rows}{The default normalizes the
    sum of squares of the rows
    in \code{abs2.rows} or \code{x.rows} to 1.  The optional value
    \code{normalize="abs2"} scales the rows
    in \code{abs2.rows} or \code{x.rows} to make the sum of all
    positive value equal 1 and the sum of all negative values equal \eqn{-1}.
    Together, the sum of the absolute values of the specified rows in
    each column is 2.
  }
  \item{Int}{logical.  Default \code{TRUE} means make all columns
    orthogonal to the constant column (Intercept in regression
    terminology).  The alternative is to use only the columns in the
    input matrix \code{x}.
  }
  \item{drop.Int}{logical.  The default is to drop the constant column
    and to keep all columns when the constant is not automatically generated.}

}
\details{

  This function is based on \code{qr.Q}.  The input matrix \code{x} has
  \code{n} rows and an arbitrary non-zero number of columns.  The result
  is an \code{n} by \code{n} orthogonal matrix.  By default the first
  column of the result is constant and is not returned.  The second
  column of the result is orthogonal to the first result column.
  Together the first two result columns span the space of the constant
  column and the first input column.  The third result column is
  orthogonal to the first two result columns and the the three result
  columns together span the space of the constant column and the first
  two inout columns.  Similarly for the remaining result columns.
  Result columns beyond the number of input columns are constructed as
  an arbitrary orthogonal completion.

  If the input columns are orthogonal to each other and to the constant
  column, then the result columns are rescaled versions of the input
  columns.

  Optionally (\code{drop.Int=FALSE}), the constant column can be returned.

  By default the columns are scaled to have sum of squares equal 1.  If
  \code{normalize="abs2"}, they are scaled to make the sum of all
  positive value equal 1 and the sum of all negative values equal \eqn{-1}.
  Together, the sum of the absolute values of each column is 2.

  If the input is a set of columns from a contrast matrix for a design
  that has multiple terms, the \code{abs2.rows} argument is used to
  specify which rows are to be included in the normalization.  These
  will normally be rows associated with one of the main effects.
}
\value{
Matrix of orthogonal columns.
}
\references{ Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{MMC}}}
\examples{
zapsmall(
orthog.complete(cbind("4-12"=c(-1,-1, 0, 2),
                      "1-2" =c( 1,-1, 0, 0)))
)

zapsmall(
orthog.complete(cbind("4-12"=c(-1,-1, 0, 2),
                      "1-2" =c( 1,-1, 0, 0)),
                drop.Int=FALSE)
)

zapsmall(
orthog.complete(cbind("4-12"=c(-1,-1, 0, 2),
                      "1-2" =c( 1,-1, 0, 0)),
                normalize="abs2")
)


## used in MMC plots
tmp <- data.frame(y=rnorm(12),
                  a=factor(c("u","u","u","u",
                             "v","v","v","v",
                             "w","w","w","w")))
tmp.aov <- aov(y ~ a, data=tmp)
lmat <- if.R(
  s=multicomp(tmp.aov, focus="a")$lmat,
  r={lmat.reduced <- t(glht(tmp.aov, linfct=mcp(a="Tukey"))$linfct)
     rbind(lmat.reduced, AU=-apply(lmat.reduced[-1,], 2, sum))
    })
zapsmall(lmat)

lmat.complete <- orthog.complete(lmat, abs2.rows=-1,
                                 normalize="abs2",
                                 drop.Int=FALSE)[,1:3]
zapsmall(lmat.complete)
if.R(r=zapsmall(lmat.complete[-4,]),
     s={})


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{algebra }
