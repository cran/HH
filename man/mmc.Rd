\name{mmc}
\alias{mmc}
\alias{MMC}
\alias{multicomp}
\alias{multicomp.mmc}
\alias{mmc}
\alias{mmc.glht}
%% \alias{mmc.lm}
\alias{mmc.default}
\alias{[.mmc.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MMC (Mean--mean Multiple Comparisons) plots.}
\description{
  Constructs a \code{"mmc.multicomp"} object from the formula and
  other arguments.  The constructed object must be explicitly plotted
  with the \code{\link{mmcplot}} function.
}
\usage{
mmc(model, ...)  ## R

\method{mmc}{glht}(model, ...)


\method{mmc}{default}(model,       ## lm object
    linfct=NULL,
    focus=
    if (is.null(linfct))
    {
      if (length(model$contrasts)==1) names(model$contrasts)
      else stop("focus or linfct must be specified.")
    }
    else
    {
      if (is.null(names(linfct)))
        stop("focus must be specified.")
      else names(linfct)
    },
    focus.lmat,
    ylabel=deparse(terms(model)[[2]]),
    lmat=if (missing(focus.lmat)) {
      t(linfct)
    } else {
      lmatContrast(t(none.glht$linfct), focus.lmat)
      },
    lmat.rows=lmatRows(model, focus),
    lmat.scale.abs2=TRUE,
    estimate.sign=1,
    order.contrasts=TRUE,
    level=.95,
    calpha=NULL,
    alternative = c("two.sided", "less", "greater"),
    ...
    )

multicomp.mmc(x,  ## S-Plus
              focus=dimnames(attr(x$terms,"factors"))[[2]][1],
              comparisons="mca",
              lmat,
              lmat.rows=lmatRows(x, focus),
              lmat.scale.abs2=TRUE,
              ry,
              plot=TRUE,
              crit.point,
              iso.name=TRUE,
              estimate.sign=1,
              x.offset=0,
              order.contrasts=TRUE,
              main,
              main2,
              focus.lmat,
              ...)

\method{[}{mmc.multicomp}(x, ..., drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{\code{"aov"} object in \code{"lm"} method.}
  \item{ylabel}{name of the response variable.}
  \item{lmat}{contrast matrix as in the S-Plus \code{multicomp}.
    The convention for \code{lmat} in R is to use
    the transpose of the \code{linfct} component produced by
    \code{glht}.   Required for user-specified contrasts.}
  \item{lmat.rows}{rows in \code{lmat} for the \code{focus} factor.}
  \item{focus}{define the factor to compute contrasts of.
In R this argument often can be used to simplify the call.
The statement \code{mmc(my.aov, focus="factorA")} is interpreted as
\code{mmc(my.aov, factorA="Tukey", `interaction_average`=TRUE, `covariate_average`=TRUE)}
With \code{TRUE, TRUE}, \code{multcomp::glht} always gives the same result as the S-Plus
\code{multcomp} function.  Without the \code{TRUE, TRUE},
\code{multcomp::glht} gives a
different answer when there are interactions or covariates in the model.
    See
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus
  \code{multicomp}.
#endif
}
\item{focus.lmat}{R only.  Contrast matrix used in the user-specified
comparisons of the \code{focus} factor.  This is the matrix the user
constructs.
Row names must include all levels of the factor.
Column names are the names the user assigns to the contrasts.
Each column must sum to zero.  See \code{catalystm.lmat} in the Examples
section for an example.
The \code{focus.lmat} matrix is multiplied by the \code{lmat} from the \code{none}
component to create the \code{lmat} for the user-specified contrasts.
Display the \code{hibrido.lmat} and \code{maiz2.lmat} in the maiz
example below to see what is happening.
 }
  \item{linfct}{In R, see
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus
  \code{multicomp}.
#endif
}
  \item{\dots}{other arguments.  \code{alternative} and
    \code{base} are frequently used with \code{glht}.}
  \item{comparisons}{argument to \code{multicomp}}
  \item{lmat.scale.abs2}{logical, scale the contrasts in the columns of
    \code{lmat} to make the sum of the absolute values of each column equal 2.}
  \item{estimate.sign}{numeric.  If \code{0}, leave contrasts in the
    default lexicographic direction.  If positive, force all contrasts to positive,
    reversing their names if needed (if contrast A-B is negative, reverse it
    to B-A).  If negative, the force all contrasts to positive.}
  \item{order.contrasts}{sort the contrasts in the (\code{mca}, \code{none},
    \code{lmat}) components by height on the MMC plot.  This will place the
    contrasts in the multicomp plots in the same order as in the MMC plot.}
  \item{alternative}{Direction of alternative hypothesis.
    See
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}
#endif
#ifdef S-Plus
  \code{glht}
#endif
 in R.  S-Plus \code{multicomp}
    uses the argument \code{bounds} for this concept.
  }
  \item{level}{Confidence level.  Defaults to 0.95.}
  \item{crit.point, calpha}{critical value for the tests.  The value from the
    specified \code{multicomp} method is used for the user-specified
    contrasts when \code{lmat} is specified.  This argument is called
    \code{crit.point} with \code{multicomp} in S-Plus and \code{calpha}
    when used with \code{glht} and \code{confint} in R.
  In R, with a large number of levels for the focus factor, \code{calpha}
  should
  be specified.  See notes below for discussion of the timing issues
and the examples for an illustration how to use \code{calpha}.}
  \item{plot}{logical, display the plot if \code{TRUE}.}
  \item{ry, iso.name, x.offset, main, main2}{arguments to
    \code{plot.mmc.multicomp}.}
  \item{x, drop}{See \code{"["}.}
}
\details{

By default, if \code{lmat} is not specified, we plot the isomeans grid
and the pairwise comparisons for the \code{focus} factor.  By default,
we plot the specified contrasts if the \code{lmat} is specified.
Each contrast is plotted at a height which is the weighted average of
the means being compared.  The weights are scaled to the sum of their
absolute values equals 2.

We get the right contrasts automatically if the aov is oneway.  If we
specify an lmat for oneway it must have a leading row of 0.

For any more complex design, we must study the \code{lmat} from the \code{mca}
component of the result to see how to construct the \code{lmat} (with the
extra rows as needed) and how to specify the \code{lmat.rows}
corresponding to the rows for the focus factor.

\code{mmc} in R works from either an \code{"glht"} object or an
  \code{"aov"} object.  \code{multicomp.mmc} in S-Plus works from an
  \code{"aov"} object.
}

\value{

  An \code{"mmc.multicomp"} object contains either the first two or all
  three of the \code{"multicomp"} components \code{mca}, \code{none},
  \code{lmat} described here.  Each \code{"multicomp"} component in
  R also contains a \code{"glht"} object.

  \item{mca}{Object containing the pairwise comparisons.}

  \item{none}{Object comparing each mean to 0.}

  \item{lmat}{Object for the contrasts specified in
    the \code{lmat} argument.}

  \code{"[.mmc.multicomp"} is a subscript method.
}
\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course with Examples in R}.
Second Edition.
Springer-Verlag, New York.
\url{https://link.springer.com/book/10.1007/978-1-4939-2122-5}

Heiberger, Richard M. and Holland, Burt (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.

Hsu, J. and Peruggia, M. (1994).
"Graphical representations of Tukey's multiple comparison method."
\emph{Journal of Computational and Graphical Statistics}, 3:143--161.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different functions.
  MMC plots in R are constructed by \code{mmc}
  based on
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus
  \code{glht}.
#endif
  MMC plots in S-Plus are constructed by
  \code{multicomp.mmc} based on the S-Plus
%% R
#ifndef S-Plus
  \code{multicomp}.
#endif
#ifdef S-Plus
  \code{\link[splus]{multicomp}}.
#endif
  The MMC plot is the same in both systems.  The details of getting the
  plot differ.

  Function \code{mmc} calls
%% R
#ifndef S-Plus
 \code{\link[multcomp]{glht}} and \code{\link[multcomp:methods]{confint.glht}}.
#endif
#ifdef S-Plus
  \code{glht} and \code{confint.glht}.
#endif
  With a large number of levels
  for the focus factor, the \code{confint} function is exceedingly slow
  (80 minutes for 30 levels on 1.5GHz Windows XP). Therefore,
  always specify \code{calpha} to reduce the time to under a second for
  the same example.


There are two plotting functions for MMC plots.  \code{\link{mmcplot}}, the
  newer \pkg{lattice}-based function, is recommended.   \code{mmcplot},
chooses better default values for it arguments and is better coordinated
  with the tiebreaker plot.

The older
  \code{\link{plot.mmc.multicomp}}, built on \pkg{base} graphics, chooses sensible defaults for its many
arguments, but they still often need manual adjustment.  The examples show
several types of adjustments.  We have changed the centering and scaling
to avoid overprinting of label information.  By default the significant
contrasts are shown in a more intense color than the nonsignificant
contrasts.
We have an option to reduce the color intensity of the isomeans grid.

  %% I have used the estimate.sign=0 argument only once ever.
  %% I did so in ~/504.s06/0316/filter.s and that is because one
  %% the response was a difference and the positive and negative values
  %% were interpretable.

}
\seealso{\code{\link{mmcplot}}, \code{\link{plot.mmc.multicomp}}, \code{\link{as.multicomp}}}

\examples{
## Use mmc with R.
## Use multicomp.mmc with S-Plus.

## data and ANOVA
## catalystm example
data(catalystm)

bwplot(concent ~ catalyst, data=catalystm,
       scales=list(cex=1.5),
       ylab=list("concentration", cex=1.5),
       xlab=list("catalyst",cex=1.5))


catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
summary(catalystm1.aov)

catalystm.mca <-
glht(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
confint(catalystm.mca)
plot(catalystm.mca)                      ## multcomp plot
mmcplot(catalystm.mca, focus="catalyst") ## HH plot

## pairwise comparisons
catalystm.mmc <-
  mmc(catalystm1.aov, focus="catalyst")
catalystm.mmc

\dontrun{
## these three statements are identical for a one-way aov
  mmc(catalystm1.aov)  ## simplest
  mmc(catalystm1.aov, focus="catalyst") ## generalizes to higher-order designs
  mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey")) ## glht arguments
}

mmcplot(catalystm.mmc, style="both")


## User-Specified Contrasts
## Row names must include all levels of the factor.
## Column names are the names the user assigns to the contrasts.
## Each column must sum to zero.
catalystm.lmat <- cbind("AB-D" =c( 1, 1, 0,-2),
                        "A-B"  =c( 1,-1, 0, 0),
                        "ABD-C"=c( 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- levels(catalystm$catalyst)
catalystm.lmat

catalystm.mmc <-
mmc(catalystm1.aov,
       linfct = mcp(catalyst = "Tukey"),
       focus.lmat=catalystm.lmat)
catalystm.mmc

mmcplot(catalystm.mmc, style="both", type="lmat")


\dontrun{
## Dunnett's test
## weightloss example
data(weightloss)
bwplot(loss ~ group, data=weightloss,
       scales=list(cex=1.5),
       ylab=list("Weight Loss", cex=1.5),
       xlab=list("group",cex=1.5))

weightloss.aov <- aov(loss ~ group, data=weightloss)
summary(weightloss.aov)

group.count <- table(weightloss$group)

tmp.dunnett <-
  glht(weightloss.aov,
       linfct=mcp(group=contrMat(group.count, base=4)),
       alternative="greater")
mmcplot(tmp.dunnett, main="contrasts in alphabetical order", focus="group")

tmp.dunnett.mmc <-
  mmc(weightloss.aov,
      linfct=mcp(group=contrMat(group.count, base=4)),
      alternative="greater")
mmcplot(tmp.dunnett.mmc,
        main="contrasts ordered by average value of the means\nof the two levels in the contrasts")

tmp.dunnett.mmc
}

\dontrun{
## two-way ANOVA
## display example

data(display)

interaction2wt(time ~ emergenc * panel.ordered, data=display)

displayf.aov <- aov(time ~ emergenc * panel, data=display)
anova(displayf.aov)

## multiple comparisons
## MMC plot
displayf.mmc <- mmc(displayf.aov, focus="panel")
displayf.mmc

## same thing using glht argument list
displayf.mmc <-
  mmc(displayf.aov,
      linfct=mcp(panel="Tukey", `interaction_average`=TRUE, `covariate_average`=TRUE))

mmcplot(displayf.mmc)


panel.lmat <- cbind("3-12"=c(-1,-1,2),
                    "1-2"=c( 1,-1,0))
dimnames(panel.lmat)[[1]] <- levels(display$panel)
panel.lmat

displayf.mmc <-
  mmc(displayf.aov, focus="panel", focus.lmat=panel.lmat)

## same thing using glht argument list
displayf.mmc <-
  mmc(displayf.aov,
      linfct=mcp(panel="Tukey", `interaction_average`=TRUE, `covariate_average`=TRUE),
      focus.lmat=panel.lmat)

mmcplot(displayf.mmc, type="lmat")
}

\dontrun{
## split plot design with tiebreaker plot
##
## This example is based on the query by Tomas Goicoa to R-news
## http://article.gmane.org/gmane.comp.lang.r.general/76275/match=goicoa
## It is a split plot similar to the one in HH Section 14.2 based on
## Yates 1937 example.  I am using the Goicoa example here because its
## MMC plot requires a tiebreaker plot.


data(maiz)

interaction2wt(yield ~ hibrido+nitrogeno+bloque, data=maiz,
               par.strip.text=list(cex=.7))
interaction2wt(yield ~ hibrido+nitrogeno, data=maiz)

maiz.aov <- aov(yield ~ nitrogeno*hibrido + Error(bloque/nitrogeno), data=maiz)

summary(maiz.aov)
summary(maiz.aov,
        split=list(hibrido=list(P3732=1, Mol17=2, A632=3, LH74=4)))

try(glht(maiz.aov, linfct=mcp(hibrido="Tukey")))  ## can't use 'aovlist' objects in glht

## R glht() requires aov, not aovlist
maiz2.aov <- aov(terms(yield ~ bloque*nitrogeno + hibrido/nitrogeno,
                       keep.order=TRUE),
                 data=maiz)
summary(maiz2.aov)

## There are many ties in the group means.
## These are easily seen in the MMC plot, where the two clusters
## c("P3747", "P3732", "LH74") and c("Mol17", "A632")
## are evident from the top three contrasts including zero and the
## bottom contrast including zero.  The significant contrasts are the
## ones comparing hybrids in the top group of three to ones in the
## bottom group of two.

## We have two graphical responses to the ties.
## 1. We constructed the tiebreaker plot.
## 2. We construct a set of orthogonal contrasts to illustrate
##    the clusters.

## pairwise contrasts with tiebreakers.
maiz2.mmc <- mmc(maiz2.aov,
                 linfct=mcp(hibrido="Tukey", interaction_average=TRUE))
mmcplot(maiz2.mmc, style="both")  ## MMC and Tiebreaker


## orthogonal contrasts
## user-specified contrasts
hibrido.lmat <- cbind("PPL-MA" =c(2, 2,-3,-3, 2),
                      "PP-L"   =c(1, 1, 0, 0,-2),
                      "P47-P32"=c(1,-1, 0, 0, 0),
                      "M-A"    =c(0, 0, 1,-1, 0))
dimnames(hibrido.lmat)[[1]] <- levels(maiz$hibrido)
hibrido.lmat

maiz2.mmc <-
  mmc(maiz2.aov, focus="hibrido", focus.lmat=hibrido.lmat)
maiz2.mmc

## same thing using glht argument list
maiz2.mmc <-
  mmc(maiz2.aov, linfct=mcp(hibrido="Tukey",
      `interaction_average`=TRUE), focus.lmat=hibrido.lmat)

  mmcplot(maiz2.mmc, style="both", type="lmat")
}
}
\keyword{hplot}
\keyword{ htest }
\keyword{design }
