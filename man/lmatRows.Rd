\name{lmatRows}
\alias{lmatRows}
\alias{lmatRows.mmc.multicomp}
\alias{lmatRows.multicomp}
\alias{lmatRows.glht}
\alias{lmatRows.lm}
\alias{lmatContrast}
\title{Find the row numbers in the lmat 
corresponding to the focus factor.}
\description{
\code{lmatRows} finds the row numbers in the lmat (column numbers in the linfct in R)
corresponding to the focus factor.  See \code{\link{mmc}} for more information.
These are internal functions that the user doesn't see.
They are necessary when the design has more than one factor.
\code{lmatContrast} converts user-specified contrasts of levels of a
factor to the full \code{lmat} or \code{linfct} matrix that carries the
information about other factors and their interactions and covariates.
}
\usage{
lmatRows(x, focus)
\method{lmatRows}{mmc.multicomp}(x, focus)
\method{lmatRows}{multicomp}(x, focus)
\method{lmatRows}{glht}(x, focus) ## R only
\method{lmatRows}{lm}(x, focus)
lmatContrast(lmat.none, contrast.matrix)
}
\arguments{
  \item{x}{\code{"lm"} or \code{"mmc.multicomp"} or \code{"multicomp"}
    or \code{"glht"} object.
  }
  \item{focus}{The name of the term in the ANOVA table for which
    multiple comparisons are to be constructed.}
  \item{lmat.none}{\code{lmat} matrix with the S-Plus \code{multicomp}
    package or \code{t(linfct)} matrix with the R \code{multcomp} package.
    In both packages the matrix is the one used for estimating the group
    means.
    }
    \item{contrast.matrix}{Matrix of column contrasts for a factor.
      The columns are the contrasts, the rows are the levels of the factor.
    }
}
\details{
The \code{MMC} function are based on \code{glht} in R and on
\code{multicomp}
in S-Plus.  The two packages have different conventions for specifying
the linear contrasts.  The \code{lmatRows} function gives appropriate
values in each system.
}
\value{
  For \code{lmatRows}, vector of row numbers of the \code{lmat},
  the matrix of linear contrasts defining the comparisons of interest.
  For \code{lmatContrast}, a linear contrast matrix that follows the
  conventions of the multiple comparisons package.  It has columns for each contrast
  specified by the input \code{contrast.matrix} and rows as needed for
  the other terms in the model.
}
\examples{
## catalystm example
## See ?MMC for more on this example
data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)

catalystm.mmc <-
  if.R(r=mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey")),
       s=multicomp.mmc(catalystm1.aov, plot=FALSE))
dimnames(catalystm.mmc$mca$lmat)[[1]]
lmatRows(catalystm1.aov, focus="catalyst")

## user-specified contrasts
catalystm.lmat <- cbind("AB-D" =c( 1, 1, 0,-2),
                        "A-B"  =c( 1,-1, 0, 0),
                        "ABD-C"=c( 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- levels(catalystm$catalyst)
zapsmall(lmatContrast(catalystm.mmc$none$lmat, catalystm.lmat))
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{mmc}},
%% R
#ifndef S-Plus
\code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus  
  \code{\link[splus]{multicomp}}.
#endif
}
\keyword{ dplot }
