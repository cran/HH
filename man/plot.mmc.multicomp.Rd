\name{plot.mmc.multicomp}
\alias{plot.mmc.multicomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MMC (Mean--mean Multiple Comparisons) plot.}
\description{
  MMC (Mean--mean Multiple Comparisons) plot.
}
\usage{
\method{plot}{mmc.multicomp}(x,
     xlab="contrast value",
     ylab=none$ylabel,
     focus=none$focus,
     main= main.method.phrase,
     main2=main2.method.phrase,
     main.method.phrase=
       paste("multiple comparisons of means of", ylab),
     main2.method.phrase=paste("simultaneous ",
       100*(1-none$alpha),"\% confidence limits, ",
       method, " method", sep="" ),
     ry.mmc=TRUE,
     key.x=par()$usr[1]+ diff(par()$usr[1:2])/20,
     key.y=par()$usr[3]+ diff(par()$usr[3:4])/3,
     method=if (is.null(mca)) lmat$method else mca$method,
     print.lmat=(!is.null(lmat)),
     print.mca=(!is.null(mca) && (!print.lmat)),
     iso.name=TRUE,
     x.offset=0,
     col.mca.signif="red",  col.mca.not.signif="black",
     lty.mca.signif=1,  lty.mca.not.signif=6,
     lwd.mca.signif=1,  lwd.mca.not.signif=1,
     col.lmat.signif="blue", col.lmat.not.signif="black",
     lty.lmat.signif=1, lty.lmat.not.signif=6,
     lwd.lmat.signif=1, lwd.lmat.not.signif=1,
     lty.iso=7, col.iso="darkgray", lwd.iso=1,
     lty.contr0=2, col.contr0="darkgray", lwd.contr0=1,
     decdigits.ybar=2,
     ...
     )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{mmc.multicomp} object}
  \item{xlab}{\code{"contrast value"}.  An alternate \code{""} can help
    unclutter a figure when several MMC plots are displayed together.}
  \item{ylab}{name of response variable}
  \item{focus}{define the factor to compute contrasts of. }
  \item{main, main2}{main and second line of title of plot}
  \item{main.method.phrase, main2.method.phrase}{default expressions for title of plot}
  \item{ry.mmc}{range of values on the y-axis.  It is similar to
    \code{par("ylim")}, but not the same as additional calculations are
    needed to maintain the isomeans grid as a square.}
  \item{key.x, key.y}{location of the key displayed when \code{iso.name=FALSE}.}
  \item{method}{method used to construct contrasts and confidence
    intervals.  See the \code{type} argument to
    \code{\link[multcomp]{glht}} for the list.}
  \item{print.lmat}{logical.  If \code{TRUE}, then display the
    user-specified contrasts.}
  \item{print.mca}{logical.  If \code{TRUE}, then display the
    pair-wise contrasts.}
  \item{iso.name}{logical.  If \code{TRUE}, label the isomeans grid with
  the factor levels.  If \code{FALSE}, label the isomeans grid with
  sequential numbers and display a key relating the numbers to the
  factor levels.}
  \item{x.offset}{amount to move the vertical 0 line to the left or
    right to reduce overprinting of labels and plotted lines.}

  \item{col.mca.signif,
        lty.mca.signif,
        lwd.mca.signif}{color, line type, line width for significant pairwise contrasts.}

  \item{col.mca.not.signif,
        lty.mca.not.signif,
        lwd.mca.not.signif}{color, line type, line width for non-significant pairwise contrasts.}

  \item{col.lmat.signif,
        lty.lmat.signif,
        lwd.lmat.signif}{color, line type,
	line width for significant user-specified contrasts.}

  \item{col.lmat.not.signif,
        lty.lmat.not.signif,
        lwd.lmat.not.signif}{color, line type,
	line width for non-significant user-specified contrasts.}

      \item{lty.iso, col.iso, lwd.iso}{color, line type,
	line width for the isomeans grid.}

      \item{lty.contr0, col.contr0, lwd.contr0}{color, line type,
	line width for the vertical contrast=0 line.}

      \item{decdigits.ybar}{number of decimal digits in the left-axis labels.}
      \item{\dots}{other arguments, currently ignored.}
}
\references{
Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R.~M. and Holland, B. (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.

  Hsu, J. and Peruggia, M. (1994).
"Graphical representations of Tukey's multiple comparison method."
\emph{Journal of Computational and Graphical Statistics}, 3:143--161.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{

  \code{plot.mmc.multicomp} chooses sensible defaults for its many
arguments.  They will often need manual adjustment.  The examples show
several types of adjustments.  We have changed the centering and scaling
to avoid overprinting of label information.  By default the significant
contrasts are shown in a more intense color than the nonsignificant
contrasts.
We have an option to reduce the color intensity of the isomeans grid.

When there is overprinting of labels (a consequence of level means being
close together), a tiebreaker plot may be needed.  See \code{?MMC} for
an example.
}
\seealso{\code{\link{mmc}}, \code{\link{plotMatchMMC}}}
\examples{
data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
summary(catalystm1.aov)

## See ?MMC to see why these contrasts are chosen
catalystm.lmat <- cbind("AB-D" =c( 1, 1, 0,-2),
                        "A-B"  =c( 1,-1, 0, 0),
                        "ABD-C"=c( 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- levels(catalystm$catalyst)


catalystm.mmc <-
if.R(r={mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"),
            focus.lmat=catalystm.lmat)}
    ,s={multicomp.mmc(catalystm1.aov, focus.lmat=catalystm.lmat,
                     plot=FALSE)}
)

\dontrun{
## pairwise contrasts, default settings
plot(catalystm.mmc, print.lmat=FALSE)
}

## Centering, scaling, emphasize significant contrasts.
## Needed in R with 7in x 7in default plot window.
## Not needed in S-Plus with 4x3 aspect ratio of plot window.
plot(catalystm.mmc, x.offset=2.1, ry.mmc=c(50,58), print.lmat=FALSE)

## user-specified contrasts
plot(catalystm.mmc, x.offset=2.1, ry.mmc=c(50,58))

## reduce intensity of isomeans grid, number isomeans grid lines
plot(catalystm.mmc, x.offset=2.1, ry.mmc=c(50,58),
     lty.iso=2, col.iso='darkgray', iso.name=FALSE)

## both pairwise contrasts and user-specified contrasts
plot(catalystm.mmc, x.offset=2.1, ry.mmc=c(50,58), lty.iso=2,
     col.iso='darkgray', print.mca=TRUE)
}
\keyword{hplot}
