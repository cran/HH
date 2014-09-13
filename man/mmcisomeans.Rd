\name{mmcisomeans}
\alias{mmcisomeans}
\alias{mmcmatch}
\alias{mmcboth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions used by mmcplot.
}
\description{
Functions used by mmcplot.
}
\usage{
mmcisomeans(mmc, type = "mca", xlim = NULL, ylim = NULL, ...,
            axis.right=2.2,
            ylab=paste(
              mmc$none$ylabel, "means",
              " | ",
              mmc$none$focus, "level"),
            ylab.right=NULL,
            xlab="contrast value",
            contrast.label=TRUE)

mmcmatch(mmc, type = "mca", xlim = NULL, ylim = NULL, ...,
         axis.right=2.2,
         ylab=NULL,
         ylab.right=NULL,
         xlab="contrast value",
         contrast.label=TRUE,
         xlim.match=(type != "none"))

mmcboth(mmc, type = "mca", h = c(0.7, 0.3), xlim = NULL, ylim = NULL, ...,
        ylab.right=NULL, MMCname="MMC", Tiebreakername="Tiebreaker")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mmc}{
    \code{mmc} object or other object as indicated by method.
  }
  \item{type}{
    One of \code{c("mca", "lmat", "linfct", "none")}.
    For the default \code{"mca"}, an MMC plot is drawn of the pairwise contrasts.
    For  \code{"lmat"} or \code{"linfct"}, an MMC plot is drawn of the
    contrasts specified to \code{mmc} in the \code{lmat} or
    \code{linfct} argument.  For \code{"none"}, a confidence interval
    plot for the group means is drawn.
}
  \item{h}{
\code{h} argument for \code{\link[latticeExtra]{resizePanels}}.
}
  \item{xlim, ylim, xlab, ylab, ylab.right}{Standard
        \code{\link[lattice]{xyplot}} arguments.
}
  \item{\dots}{
Other arguments, to be forwarded to methods.
}
  \item{axis.right}{
Value used internally for\cr
\code{par.settings=list(layout.widths=list(axis.right=axis.right))}.
The user may need to set this in two circumstances.  First, if the
contrast names overflow the right edge of the plotting window, then use
        a larger value than the default.
Second, if there is a \code{ylab.right} and it is too far away from the
figure,
then use a smaller value than the default.
}
  \item{contrast.label}{
Logical.  The default \code{TRUE} means place the word \code{contrasts}
at the bottom of the right axis under the tick labels.
\code{FALSE} means don't place anything there.
}
  \item{MMCname, Tiebreakername}{
Panel names when \code{mmcplot} is used with \code{style="both"}.
}
  \item{xlim.match}{
Logical.  If \code{TRUE}, use \code{xlim} based on the contrasts in the \code{mca}
component.  If \code{FALSE}, use \code{xlim} based on the values of the
estimates
in the current component.
If
}
}
\value{
A \code{"trellis"} object.
}
\references{
See \code{\link{mmc}} for the references.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link{mmc}} for the discussion of the MMC.
  \code{\link{mmcplot}} for the user calls that get executed by the
        functions documented here.
}
\examples{
\dontrun{
  ## these examples exercise all optional arguments
data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)

catalystm.glht <-
   glht(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
confint(catalystm.glht)

plot(catalystm.glht)  ## this is the multcomp:::plot.glht

mmcplot(catalystm.glht)  ## mmcplot.glht sends its argument to HH:::as.multicomp.glht with
## the default arguments (estimate.sign = 1, order.contrasts = TRUE) unless overridden:
##
mmcplot(catalystm.glht, order.contrasts=FALSE, estimate.sign=0, main="B'")

catalystm.lmat <- cbind("AB-D" =c(1, 1, 0,-2),
                        "A-B"  =c(1,-1, 0, 0),
                        "ABD-C"=c(1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- levels(catalystm$catalyst)
catalystm.mmc <-
   mmc(catalystm1.aov,
       linfct = mcp(catalyst = "Tukey"),
       focus.lmat=catalystm.lmat)

mmcplot(catalystm.mmc, type="mca",  style="confint")
mmcplot(catalystm.mmc, type="lmat", style="confint")
mmcplot(catalystm.mmc, type="none", style="confint")
mmcplot(catalystm.mmc, type="none", style="confint", xlim.match=FALSE,
        main="xlim.match=FALSE is default for none confint")
mmcplot(catalystm.mmc, type="none", style="confint", xlim.match=TRUE, main="out of bounds")

mmcplot(catalystm.mmc$mca,  style="confint")
mmcplot(catalystm.mmc$lmat, style="confint")
mmcplot(catalystm.mmc$none, style="confint")

plot(catalystm.mmc) ## HH:::plot.mmc.multicomp method

mmcplot(catalystm.mmc)

mmcplot(catalystm.mmc)
mmcplot(catalystm.mmc, style="isomeans")
mmcplot(catalystm.mmc, style="confint")
mmcplot(catalystm.mmc, style="both")

mmcplot(catalystm.mmc, style="isomeans", type="mca")
mmcplot(catalystm.mmc, style="isomeans", type="lmat")
mmcplot(catalystm.mmc, style="isomeans", type="linfct")
mmcplot(catalystm.mmc, style="isomeans", type="none")
mmcplot(catalystm.mmc, style="isomeans", type="none", xlim.match=FALSE)

mmcplot(catalystm.mmc, style="confint",  type="mca")
mmcplot(catalystm.mmc, style="confint",  type="lmat")
mmcplot(catalystm.mmc, style="confint",  type="linfct")
mmcplot(catalystm.mmc, style="confint",  type="none")
mmcplot(catalystm.mmc, style="confint",  type="none", xlim.match=FALSE)

mmcplot(catalystm.mmc, style="both",     type="mca")
mmcplot(catalystm.mmc, style="both",     type="lmat")
mmcplot(catalystm.mmc, style="both",     type="linfct")
mmcplot(catalystm.mmc, style="both",     type="none")
mmcplot(catalystm.mmc, style="both",     type="none", xlim.match=FALSE)

mmcplot(catalystm.mmc$mca)
mmcplot(catalystm.mmc$mca$glht)
mmcplot(catalystm.mmc$none)
mmcplot(catalystm.mmc$none$glht)
mmcplot(catalystm.mmc$lmat)
mmcplot(catalystm.mmc$lmat$glht)

mmcplot(catalystm.mmc, type="none")
mmcplot(catalystm.mmc, type="none", xlim.match=FALSE)
mmcplot(catalystm.mmc$none)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
