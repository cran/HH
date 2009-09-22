\name{plot.multicomp}
\alias{plot.multicomp}
\alias{plot.multicomp.hh}
\alias{plot.matchMMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple comparisons plot that gives independent user control
  over the appearance of the significant and not significant comparisons.
}
\description{Multiple comparisons plot that gives independent user control
  over the appearance of the significant and not significant
  comparisons.
  In R, both \code{plot.multicomp} \code{plot.multicomp.hh} coerce their argument
  to an \code{"glht"} object and plots
  that with the appropriate \code{plot} method. 
  In S-Plus,
  \code{plot.multicomp.hh} augments the standard \code{plot.multicomp} to
  give additional user arguments to control the appearance of the plot.
  
  \code{plot.matchMMC} uses the \code{plot.multicomp.hh} code.
   \code{plot.matchMMC} must immediately follow a plot of an
  \code{mmc.multicomp} object and is applied to either the \code{$mca}
  or \code{$lmat} component of the \code{mmc.multicomp} object.
  \code{plot.matchMMC} is used as a tiebreaker plot for the \code{MMC}
  plot.  \code{plot.matchMMC} matches the horizontal scaling of the
  \code{MMC} plot and displays the individual contrasts in the same
  order as the \code{MMC} plot.  See \code{\link{mmc}} for examples.
  
}
\usage{
\method{plot}{multicomp}(x, ...) ## R only

\method{plot}{multicomp.hh}(x, ylabel = x$ylabel, href = 0, uniform = TRUE,
           plt.in = c(0.2, 0.9, 0.1, 0.9),
           x.label.adj=1,
           xrange.include=href,
           xlim,
           comparisons.per.page=21,
           col.signif=1, col.not.signif=1,
           lty.signif=4, lty.not.signif=4,
           lwd.signif=1, lwd.not.signif=1,
           ...,
           xlabel.print=TRUE, y.axis.side=2, ylabel.inside=FALSE)

plot.matchMMC(x, ...,
              xlabel.print=FALSE,
              cex.axis=par()$cex.axis,
              col.signif='red', main="",
              ylabel.inside=FALSE,
              y.axis.side=4)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{"multicomp"} object.}
  \item{ylabel}{Y label on graph.}
\item{y.axis.side}{Y labels are on the left by default when plotting a
  \code{"multicomp"} object.  We move them to the
  right when matching the x-axis of an MMC plot.}
  \item{\dots}{other arguments to \code{plot.multicomp}.}
  \item{ylabel.inside}{Logical value, if \code{FALSE} (the default), the
    \code{plot.matchMMC} right-axis labels are in the margin.  If
    \code{TRUE}, the right-axis labels are in the figure area.
    Setting the argument to
    \code{TRUE} makes sense when plotting the \code{lmat} component of an
    \code{mmc.multicomp} object.}
  %plot.multicomp
  \item{href}{reference line for the intervals. The default is 0. S-Plus only.}
  \item{xrange.include}{\code{xlim}
    will be extended to include these values. S-Plus only.}
  \item{uniform}{S-Plus only.  Logical value, if \code{TRUE} and the plots fill
    more than one page, the scale will be uniform across pages.}
  \item{plt.in}{S-Plus only.  Value for \code{par("plt")} to make better
  use of the space on the plotting page.}
  \item{x.label.adj}{S-Plus only.  This is the \code{par("adj")} applied
    to the x-location of the y.labels on the \code{multicomp} plot.}
  \item{xlim}{x-range of the plot.}
  \item{comparisons.per.page}{The default S-Plus \code{plot.multicomp}
    hardwires this to 21, which allows
    for all pairwise comparisons of 7 levels taken 2 at a time.
    The HH \code{plot.multicomp} makes it a variable.
    Use it together with \code{plt.in} to make better use of the space
    on the plot.  S-Plus only.}
  \item{lty.signif, lwd.signif}{Line type, and line width for
    significant comparisons. S-Plus only.}
  \item{col.signif}{Color for significant comparisons. S-Plus only for
    \code{plot.multicomp}.  Both R and S-Plus for \code{plot.matchMMC}.}
  \item{col.not.signif, lty.not.signif, lwd.not.signif}{Color, line
    type, and line width for non-significant comparisons. S-Plus only.}
  \item{xlabel.print}{logical.  When \code{TRUE}, the caption under the
    plot is printed.  When \code{FALSE}, the caption under the plot is not
    printed.  It is helpful to set this to \code{FALSE} when
    the \code{multicomp}
    plot is used as a tiebreaker plot for the MMC plot.  S-Plus only.}
   \item{cex.axis}{\code{cex} for axis ticklabels.}
   \item{main}{Main title for plot.}
}
\value{
\code{plot.multicomp} plots a \code{"multicomp"} object.  In S-Plus, this
masks the standard \code{plot.multicomp} in order to provide additional
arguments for controlling the appearance.  It defaults to the standard
appearance.  In R, it coerces its argument to an \code{"glht"} object and plots
that with the appropriate \code{plot} method.
}

\references{
  Heiberger, Richard M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.

Heiberger, R. M. and Holland, B. (2006).
"Mean--mean multiple comparison displays for families of linear contrasts."
\emph{Journal of Computational and Graphical Statistics}, 15:937--955.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{The multiple comparisons calculations in R and S-Plus use
  completely different libraries.
%% R
#ifndef S-Plus
  Multiple comparisons in R are based on \code{\link[multcomp]{glht}}.
  Multiple comparisons in S-Plus are based on \code{multicomp}.
#endif
#ifdef S-Plus  
  Multiple comparisons in R are based on \code{glht}.
  Multiple comparisons in S-Plus are based on \code{\link[splus]{multicomp}}.
#endif
  The MMC plot in the HH library is the same in both systems.
  The details of getting the plot differ.
}
\seealso{\code{\link{mmc}} in both languages,
%% R
#ifndef S-Plus
  \code{\link[multcomp]{glht}}.
#endif
#ifdef S-Plus  
  \code{\link[splus]{multicomp}}.
#endif
}
\examples{
## data and ANOVA
catalystm <- read.table(hh("datasets/catalystm.dat"), header=FALSE,
                       col.names=c("catalyst","concent"))
catalystm$catalyst <- factor(catalystm$catalyst, labels=c("A","B","C","D"))

catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
summary(catalystm1.aov)

catalystm.mca <-
if.R(r=glht(catalystm1.aov, linfct = mcp(catalyst = "Tukey")),
     s=multicomp(catalystm1.aov, plot=FALSE))
if.R(s=plot(catalystm.mca),
     r=plot(confint(catalystm.mca, calpha=qtukey(.95, 4, 12)/sqrt(2))))
       ## calpha is strongly recommended in R with a large number of levels
       ## See ?MMC for details.
}
\keyword{ dplot }
