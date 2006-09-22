\name{simint.mmc}
\alias{simint.mmc}
\alias{mmc}
\alias{MMC}
\alias{multicomp}
\alias{multicomp.mmc}
\alias{[.mmc.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MMC (mean--mean multiple comparisons) plots.}
\description{
  Constructs a \code{"mmc.multicomp"} object from the formula and
  other arguments.  The object must be explicitly plotted.
}
\usage{
simint.mmc(y,  ## R
           data,
           type = "Tukey",
           lmat=NULL,
           lmat.rows=2:nrow(mca.lmat),
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           whichf,
           cmatrix=t(lmat),
           covariates,
           ...)

multicomp.mmc(..., comparisons="mca",  ##S-Plus
              lmat, lmat.rows=-1,
              lmat.scale.abs2=TRUE,
              ry,
              plot=TRUE,
              crit.point,
              iso.name=TRUE,
              estimate.sign=1,
              x.offset=0,
              order.contrasts=TRUE,
              main,
              main2)

"[.mmc.multicomp"(x, ..., drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Analysis of variance formula.}
  \item{data}{\code{data.frame}}
  \item{type}{type of contrasts.  See \code{\link[multcomp]{simint}} for
  the list.}
  \item{lmat}{contrast matrix as in the S-Plus \code{multicomp}.
    The convention for \code{lmat} in R is to use
    the transpose of the \code{cmatrix} component produced by
    \code{simint}.   Required for user-specified contrasts.}
  \item{lmat.rows}{rows in \code{lmat} for the \code{whichf} factor.}
  \item{whichf}{define the factor to compute contrasts of.
    See \code{\link[multcomp]{simint}}.  This argument is called
    \code{focus} in \code{multicomp}.}
  \item{cmatrix}{transpose of the \code{"lmat"} argument.}
  \item{covariates}{The current version of \code{multcomp} (0.4-8
    in R-2.3.1) doesn't handle covariates correctly.}
  \item{\dots}{other arguments.  \code{alternative} and
  \code{base} are frequently used with \code{simint}.}
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
\item{crit.point}{critical value for the tests.  The value from the
  specified \code{multicomp} method is used for the user-specified
  contrasts when \code{lmat} is specified.  This argument is not
  available with \code{simint in R.}}
\item{plot}{logical, display the plot if \code{TRUE}.}
\item{ry, iso.name, x.offset, main, main2}{arguments to
  \code{plot.mmc.multicomp}.}
\item{x, drop}{See \code{"["}.}
}
\details{
By default we plot the isomeans grid and the pairwise comparisons.
We get the right contrasts automatically if the aov is oneway.
If we specify an lmat for oneway it must have a leading row of 0.

For any more complex design, we must study the lmat from the mca
component of the result to see how to construct the lmat (with the
extra rows as needed) and how to specify the lmat.rows
corresponding to the rows for the focus factor.

\code{simint} in R \code{multcomp} version 0.4-8 doesn't work correctly
with covariates.

\code{simint.mmc} works from a formula, not from an \code{"aov"} object as
  \code{multicomp.mmc} in S-Plus does.
}
\value{In R, an \code{"mmc.multicomp"} object containing either the first
  three, or all five, of the following components:
  \item{mca}{\code{"multicomp"} object containing the pairwise comparisons.}
  \item{none}{\code{"multicomp"} object comparing each mean to 0.}
  \item{hmtest}{\code{"hmtest"} object from \code{simint} for the
    pairwise comparisons.}
  \item{lmat}{\code{"multicomp"} object for the contrasts specified in
    the \code{lmat} argument.}
  \item{lmat.hmtest}{\code{"hmtest"} object from \code{simint} for the
   contrasts specified in the \code{lmat} argument.}

In S-Plus, an \code{"mmc.multicomp"} object containing either the first
  two or all three of the components (\item{mca}, \item{none},
  \item{lmat}) as described above.

 \code{"[.mmc.multicomp"} is a subscript method.
}
\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006, accepted).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}.

Hsu, J. and Peruggia, M. (1994).
"Graphical representations of {Tukey's} multiple comparison method."
\emph{Journal of Computational and Graphical Statistics}, 3:143--161.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different functions.
  MMC plots in R are constructed by \code{simint.mmc}
  based on  \code{\link[multcomp]{simint}}.
  MMC plots in S-Plus are constructed by 
  \code{multicomp.mmc} based on the S-Plus \code{\link[splus]{multicomp}}.
  The MMC plot is the same in both systems.  The details of getting the
  plot differ.
  
  %% I have used the estimate.sign=0 argument only once ever.
  %% I did so in ~/504.s06/0316/filter.s and that is because one
  %% the response was a difference and the positive and negative values
  %% were interpretable.

}
\seealso{\code{\link{as.multicomp}}, \code{\link{plot.mmc.multicomp}}}

\examples{
## simint is strictly for R.  Use multicomp.mmc with S-Plus.

## data and ANOVA
catalystm <- read.table(hh("datasets/catalystm.dat"), header=FALSE,
                       col.names=c("catalyst","concent"))
catalystm$catalyst <- factor(catalystm$catalyst, labels=c("A","B","C","D"))

if.R(r=
  bwplot(concent ~ catalyst, data=catalystm,
         scales=list(cex=1.5),
         ylab=list("concentration", cex=1.5),
         xlab=list("catalyst",cex=1.5))
,s=
t(bwplot(catalyst ~ concent, data=catalystm,
         scales=list(cex=1.5),
         xlab=list("concentration", cex=1.5),
         ylab=list("catalyst",cex=1.5)))
)


catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)

catalystm.mca <-
if.R(r=simint(concent ~ catalyst, data=catalystm, type="Tukey"),
     s=multicomp(catalystm1.aov, plot=F))
plot(catalystm.mca)
catalystm.mca


## pairwise comparisons
catalystm.mmc <-
if.R(r=simint.mmc(concent ~ catalyst, data=catalystm),
     s=multicomp.mmc(catalystm1.aov, plot=F))
if.R(r=catalystm.mmc <-
         multicomp.label.change(catalystm.mmc, "catalyst", ""),
     s={})
catalystm.mmc
plot(catalystm.mmc)
if.R(r=plot(catalystm.mmc, col.mca.signif="red"),
     s={})
plot(catalystm.mmc$mca)
plot(catalystm.mmc$none)

  ### $none works in S-Plus for all designs
  ### $none works in R for oneway ANOVA, not sure yet beyond that.



## user-specified contrasts
catalystm.lmat <- cbind("AB-D" =c(0, 1, 1, 0,-2),
                        "A-B"  =c(0, 1,-1, 0, 0),
                        "ABD-C"=c(0, 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- dimnames(catalystm.mmc$mca$lmat)[[1]]
zapsmall(catalystm.lmat)
if.R(s=dimnames(catalystm.mca$lmat)[[1]],
     r=c("(Intercept)", dimnames(catalystm.mca$cmatrix)[[2]][-1]))

if.R(r={
        catalystm.mmc <- simint.mmc(concent ~ catalyst, data=catalystm,
                          lmat=catalystm.lmat, lmat.rows=2:5,
                          type="Tukey", whichf="catalyst")
        catalystm.mmc <- multicomp.label.change(catalystm.mmc, "catalyst", "")
       },
     s={
        catalystm.mmc <- multicomp.mmc(catalystm1.aov, lmat=catalystm.lmat,
                                       plot=FALSE)
       })

catalystm.mmc
plot(catalystm.mmc)
if.R(r=plot(catalystm.mmc, col.lmat.signif="red"),
     s={})

plot(catalystm.mmc$mca)
plot(catalystm.mmc$none)
plot(catalystm.mmc$lmat)




## Dunnett's test
weightloss <- read.table(hh("datasets/weightloss.dat"), header=TRUE)
weightloss <- data.frame(loss=unlist(weightloss),
                         group=rep(names(weightloss), rep(10,5)))
if.R(r=
bwplot(loss ~ group, data=weightloss,
       scales=list(cex=1.5),
       ylab=list("Weight Loss", cex=1.5),
       xlab=list("group",cex=1.5))
,s=
t(bwplot(group ~ loss, data=weightloss,
       scales=list(cex=1.5),
       xlab=list("Weight Loss", cex=1.5),
       ylab=list("group",cex=1.5)))
)

weightloss.aov <- aov(loss ~ group, data=weightloss)
summary(weightloss.aov)

tmp.dunnett <- 
if.R(r=
simint(loss ~ group, data=weightloss,
       type="Dunnett", alternative="greater", base=4)
,s=
multicomp(weightloss.aov,
          method="dunnett", comparisons="mcc",
          bounds="lower", control=4,
          valid.check=FALSE)
)
plot(tmp.dunnett)

if.R(r={
   tmp.dunnett.mmc <-
        simint.mmc(loss ~ group, data=weightloss,
                   type="Dunnett", alternative="greater", base=4)
   tmp.dunnett.mmc <- multicomp.label.change(tmp.dunnett.mmc, "group", "")
},s=
tmp.dunnett.mmc <- 
   multicomp.mmc(weightloss.aov,
                 method="dunnett", comparisons="mcc",
                 bounds="lower", control=4,
                 valid.check=FALSE, plot=FALSE)
)

tmp.dunnett.mmc
plot(tmp.dunnett.mmc)
}
\keyword{hplot}
\keyword{ htest }
\keyword{design }
