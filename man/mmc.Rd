\name{mmc}
\alias{mmc}
\alias{MMC}
\alias{multicomp}
\alias{multicomp.mmc}
\alias{glht.mmc}
\alias{glht.mmc.glht}
\alias{glht.mmc.lm}
\alias{[.mmc.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MMC (mean--mean multiple comparisons) plots.}
\description{
  Constructs a \code{"mmc.multicomp"} object from the formula and
  other arguments.  The object must be explicitly plotted.
}
\usage{
glht.mmc(model, ...)  ## R

\method{glht.mmc}{glht}(model, ...)


\method{glht.mmc}{lm}(model,       ## lm object
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
           ylabel=as.character(terms(model)[[2]]),
           lmat=t(linfct),
           lmat.rows=-1,
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           level=.95,
           calpha=NULL,
           alternative = c("two.sided", "less", "greater"),
           ...
           )
	   
multicomp.mmc(..., comparisons="mca",  ## S-Plus
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
  \item{model}{\code{"aov"} object in \code{"lm"} method.}
  \item{ylabel}{name of the response variable.}
  \item{lmat}{contrast matrix as in the S-Plus \code{multicomp}.
    The convention for \code{lmat} in R is to use
    the transpose of the \code{linfct} component produced by
    \code{glht}.   Required for user-specified contrasts.}
  \item{lmat.rows}{rows in \code{lmat} for the \code{focus} factor.}
  \item{focus}{define the factor to compute contrasts of.
    See \code{\link[multcomp]{mcp}} in R.}
  \item{linfct}{In R, see \code{\link[multcomp]{glht}}.}
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
    See \code{\link[multcomp]{confint}} in R.  S-Plus \code{multicomp}
    uses the argument \code{bounds} for this concept.
  }
  \item{level}{Confidence level.  Defaults to 0.95.}
  \item{crit.point, calpha}{critical value for the tests.  The value from the
    specified \code{multicomp} method is used for the user-specified
    contrasts when \code{lmat} is specified.  This argument is called
    \code{crit.point} with \code{multicomp} in S-Plus and \code{calpha}
    when used with \code{glht} and \code{confint} in R.}
  \item{plot}{logical, display the plot if \code{TRUE}.}
  \item{ry, iso.name, x.offset, main, main2}{arguments to
    \code{plot.mmc.multicomp}.}
  \item{x, drop}{See \code{"["}.}
}
\details{

By default, if \code{lmat} is not specified, we plot the isomeans grid
and the pairwise comparisons for the \code{focus} factor.  By default,
we plot the specified contrasts if the \code{lmat} is specified.

We get the right contrasts automatically if the aov is oneway.  If we
specify an lmat for oneway it must have a leading row of 0.

For any more complex design, we must study the \code{lmat} from the \code{mca}
component of the result to see how to construct the \code{lmat} (with the
extra rows as needed) and how to specify the \code{lmat.rows}
corresponding to the rows for the focus factor.

\code{glht.mmc} in R works from either an \code{"glht"} object or an
  \code{"aov"} object.  \code{multicomp.mmc} in S-Plus works from an
  \code{"aov"} object.
}

\value{

  An \code{"mmc.multicomp"} object contains either the first two or all
  three of the \code{"multicomp"} components \item{mca}, \item{none},
  \item{lmat} described here.  Each  \code{"multicomp"} component in
  R also contains a \code{"glht"} object.

  \item{mca}{Object containing the pairwise comparisons.}

  \item{none}{Object comparing each mean to 0.}

  \item{lmat}{Object for the contrasts specified in
    the \code{lmat} argument.}

  \code{"[.mmc.multicomp"} is a subscript method.
}
\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006, December).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}.

Hsu, J. and Peruggia, M. (1994).
"Graphical representations of {Tukey's} multiple comparison method."
\emph{Journal of Computational and Graphical Statistics}, 3:143--161.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different functions.
  MMC plots in R are constructed by \code{glht.mmc}
  based on  \code{\link[multcomp]{glht}}.
  MMC plots in S-Plus are constructed by 
  \code{multicomp.mmc} based on the S-Plus \code{\link[splus]{multicomp}}.
  The MMC plot is the same in both systems.  The details of getting the
  plot differ.
  

  \code{plot.mmc.multicomp} chooses sensible defaults for its many
arguments.  They will often need manual adjustment.  The examples show
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
\seealso{\code{\link{as.multicomp}}, \code{\link{plot.mmc.multicomp}}}

\examples{
## Use glht.mmc with R.
## Use multicomp.mmc with S-Plus.

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
if.R(r=glht(catalystm1.aov, linfct = mcp(catalyst = "Tukey")),
     s=multicomp(catalystm1.aov, plot=FALSE))
plot(catalystm.mca)
catalystm.mca


## pairwise comparisons
catalystm.mmc <-
if.R(r=glht.mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey")),
     s=multicomp.mmc(catalystm1.aov, plot=FALSE))
catalystm.mmc
plot(catalystm.mmc)
plot(catalystm.mmc$mca)
plot(catalystm.mmc$none)



## user-specified contrasts
catalystm.lmat <- cbind("AB-D" =c(0, 1, 1, 0,-2),
                        "A-B"  =c(0, 1,-1, 0, 0),
                        "ABD-C"=c(0, 1, 1,-3, 1))
if.R(r=catalystm.lmat <- catalystm.lmat[-2,],
     s={})
dimnames(catalystm.lmat)[[1]] <- dimnames(catalystm.mmc$mca$lmat)[[1]]
zapsmall(catalystm.lmat)
if.R(s=dimnames(catalystm.mca$lmat)[[1]],
     r=dimnames(catalystm.mca$linfct)[[2]])

catalystm.mmc <-
if.R(r=glht.mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"),
                lmat=catalystm.lmat)
    ,s=multicomp.mmc(catalystm1.aov, lmat=catalystm.lmat,
                     plot=FALSE)
)

catalystm.mmc
plot(catalystm.mmc)

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

if.R(r={
group.count <- table(weightloss$group)
},s={})

tmp.dunnett <- 
if.R(r=
glht(weightloss.aov,
     linfct=mcp(group=contrMat(group.count, base=4)),
     alternative="greater")
,s=
multicomp(weightloss.aov,
          method="dunnett", comparisons="mcc",
          bounds="lower", control=4,
          valid.check=FALSE)
)
plot(tmp.dunnett)

tmp.dunnett.mmc <-
if.R(r=
   glht.mmc(weightloss.aov,
            linfct=mcp(group=contrMat(group.count, base=4)),
            alternative="greater")
,s=
   multicomp.mmc(weightloss.aov,
                 method="dunnett", comparisons="mcc",
                 bounds="lower", control=4,
                 valid.check=FALSE, plot=FALSE)
)

tmp.dunnett.mmc
plot(tmp.dunnett.mmc)



## two-way ANOVA
display <- read.table(hh("datasets/display.dat"), header=TRUE)
display$panel <- factor(display$panel)  ## display$panel <- positioned(display$panel, value=(1:3)+.5)
display$emergenc <- factor(display$emergenc)

displayf.aov <- aov(time ~ emergenc * panel, data=display)
anova(displayf.aov)

## multiple comparisons
tmp <- if.R(
   r=glht(displayf.aov, linfct=mcp(panel="Tukey")),
   s=multicomp(displayf.aov, "panel", plot=FALSE))
zapsmall(
  if.R(r=t(tmp$linfct),
       s=tmp$lmat)
)

## MMC plot
displayf.mmc <-
if.R(r=glht.mmc(displayf.aov, linfct=mcp(panel="Tukey"), focus="panel", lmat.rows=5:6),
     s=multicomp.mmc(displayf.aov, "panel", lmat.rows=6:8, plot=FALSE))
plot(displayf.mmc)

## orthogonal contrasts
zapsmall(mca.lmat <- displayf.mmc$mca$lmat)
panel.lmat <- cbind("3-12"=mca.lmat[,1] + mca.lmat[,2],
                     "1-2"=mca.lmat[,3])
displayf.mmc <-
if.R(r=glht.mmc(displayf.aov, linfct=mcp(panel="Tukey"), focus="panel",
                lmat.rows=5:6, lmat=panel.lmat),
     s=multicomp.mmc(displayf.aov, "panel", lmat.rows=6:8,
                     lmat=panel.lmat, plot=FALSE))
plot(displayf.mmc)



## split plot design with tiebreaker plot
##
## This example is based on the query by Tomas Goicoa to R-news
## http://article.gmane.org/gmane.comp.lang.r.general/76275/match=goicoa
## It is a split plot similar to the one in HH Section 14.2 based on
## Yates 1937 example.  I am using the Goicoa example here because its
## MMC plot requires a tiebreaker plot.



maiz <- read.table(hh("datasets/maiz.dat"), header=TRUE)
maiz$hibrido <- factor(maiz$hibrido,
                       levels=c("P3747","P3732","Mol17","A632","LH74"))
maiz$nitrogeno <- factor(maiz$nitrogeno)
position(maiz$nitrogeno) <- c(1, 2.5, 4, 5.5) ## forces class="ordered"

interaction2wt(yield ~ hibrido+nitrogeno+bloque, data=maiz)
interaction2wt(yield ~ hibrido+nitrogeno, data=maiz)

maiz.aov <- aov(yield ~ nitrogeno*hibrido + Error(bloque/nitrogeno), data=maiz) 

summary(maiz.aov)
summary(maiz.aov,
        split=list(hibrido=list(P3732=1, Mol17=2, A632=3, LH74=4)))

## multicomp(maiz.aov, focus="hibrido")         ## can't use 'aovlist' objects
## glht(maiz.aov, linfct=mcp(hibrido="Tukey"))  ## can't use 'aovlist' objects

sapply(maiz[-1], contrasts)
if.R(r={
  ## R glht.mmc requires treatment contrasts
  contrasts(maiz$nitrogeno) <- "contr.treatment"
  sapply(maiz[-1], contrasts)
},
     s={})

## Both R and S-Plus require aov, not aovlist
maiz2.aov <- aov(terms(yield ~ bloque*nitrogeno + hibrido/nitrogeno,
                       keep.order=TRUE), data=maiz)
summary(maiz2.aov)

if.R(s={
  maiz2.mca <- multicomp(maiz2.aov, focus="hibrido")
  ## plot(maiz2.mca)
  dimnames(maiz2.mca$lmat)[[1]]
  maiz2.mmc <- multicomp.mmc(maiz2.aov, focus="hibrido",
                             lmat.rows=16:20, plot=FALSE)
  old.mar <- par(mar=c(15,4,4,7)+.1)
  plot(maiz2.mmc)
  par(mar=c(2,4,28,7)+.1, new=TRUE)
  old.cex <- par(cex=.8)
  plot(maiz2.mmc$mca, col.signif=8, lty.signif=1, xlabel.print=FALSE,
       xaxs="d", plt=par()$plt+c(0,0,-.25,.05), xrange.include=c(-30,40))
  par(old.cex)
  par(old.mar)
},r={
  maiz2.mca <- glht(maiz2.aov, linfct=mcp(hibrido="Tukey"))
  dimnames(maiz2.mca$linfct)[[2]]
  maiz2.mmc <- glht.mmc(maiz2.aov, linfct=mcp(hibrido="Tukey"), lmat.rows=9:12)
  old.oma <- par(oma=c(12,3,0,4))
  plot(maiz2.mmc)
  par(oma=c(0,3,22,4), new=TRUE)
  plot(maiz2.mmc$mca,
       xlim=par()$usr[1:2], xaxs="i",
       main="", xlab="", cex.axis=.7)
  par(old.oma)
})


}
\keyword{hplot}
\keyword{ htest }
\keyword{design }
