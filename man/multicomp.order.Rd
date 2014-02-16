\name{multicomp.order}
\alias{multicomp.order}
\alias{multicomp.label.change}
\alias{multicomp.label.change.multicomp}
\alias{multicomp.label.change.mmc.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Update a multicomp object by ordering its contrasts.}
\description{
  Update a multicomp object by ordering its contrasts.
  The default \code{sort.by = "height"} matches the order in the MMC plot.
  An alternate \code{sort.by = "estimate"} matches the order of the
  half-normal plot.  Or the argument \code{sort.order} can be used
  to specify any other order.
}
\usage{
multicomp.order(mca, sort.by = "height", sort.order = NULL)

multicomp.label.change(x, old="adj", new="new", how.many=2)

\method{multicomp.label.change}{multicomp}(x, old="adj", new="new", how.many=2)

\method{multicomp.label.change}{mmc.multicomp}(x, old="adj", new="new", how.many=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mca}{\code{"multicomp"} object.  This is the result of
  \code{multicomp} in S-Plus or the result from applying \code{as.multicomp} to
  a \code{"glht"} object in R.}
  \item{sort.by}{Either \code{"height"} or \code{"estimate"}.}
  \item{sort.order}{Vector of indices by which the contrasts are to be
    sorted.  When \code{sort.order} in non-\code{NULL}, it is used.}

  %% multicomp.label.change
  \item{x}{\code{"multicomp"} object.}
  \item{old}{character string to be removed from contrast names.}
  \item{new}{replacement character string to be inserted in contrast names.}
  \item{how.many}{number of times to make the replacement.}

}
\value{
  The result is a \code{"multicomp"} object containing the same
  contrasts as the argument.\cr
  \code{multicomp.order} sorts the contrasts
  (and renames them consistently) according to the specifications.\cr
  \code{multicomp.label.change}
  changes the contrast names according to the specifications.

When \code{sort.by=="height"}, sort the contrasts by the reverse order
of the heights.  This provides a \code{"multicomp"} object that will be
plotted by \code{plot.multicomp} in the same order used by\cr
\code{plot.mmc.multicomp}.  If there is not \code{"height"} component,
the original \code{"multicomp"} object is returned.

When \code{sort.by=="estimate"}, sort the contrasts by the reverse order
of the contrast estimates.  This provides the same order as the
half-normal plot.

When \code{sort.order} in non-\code{NULL}, sort the contrasts in
that order.
}

\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{
  S-Plus use the \code{multicomp} functions
  and R uses the \code{multcomp} package.
}
\seealso{\code{\link{MMC}}, \code{\link{as.glht} in R},
  \code{\link{multicomp.reverse}}}
\examples{
## continue with the example in mmc in R, or multicomp.mmc in S-Plus
data(catalystm)

catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)

if.R(r={
catalystm.mca <-
   glht(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
print(confint(catalystm.mca))

catalystm.mmc <-
   mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
## the contrasts have been ordered by height (see ?MMC),
## which in this example corresponds to sort.order=c(1,2,4,3,5,6),
## and reversed, to make the contrast Estimates positive.
print(as.glht(catalystm.mmc$mca))

## ## For consistency with the S-Plus example,
## ## we change all factor level "A" to "control".
## as.glht(multicomp.label.change(catalystm.mmc$mca, "A", "control"))
},s={
catalystm.mca <-
   multicomp(catalystm1.aov, method="Tukey")
print(catalystm.mca)

catalystm.mmc <-
   multicomp.mmc(catalystm1.aov, method="Tukey", plot=FALSE)
## the contrasts have been ordered by height (see ?MMC),
## which in this example corresponds to sort.order=c(1,2,4,3,5,6),
## and reversed, to make the contrast Estimates positive.
print(catalystm.mmc$mca)

## S-Plus multicomp already uses simple names.  This function is
## therefore used in more complex two-way ANOVA examples.  We illustrate
## here by changing all factor level "A" to "control".
print(multicomp.label.change(catalystm.mmc$mca, "A", "control"))
})

}
\keyword{ dplot }
