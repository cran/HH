\name{simint.mean}
\alias{simint.mean}
\alias{simint.mmc.mean}
\alias{multicomp.mean}
\alias{multicomp.mmc.mean}

\title{ MMC  (mean--mean multiple comparisons) plots from the sufficient
statistics for a one-way design.}
\description{
  Constructs a \code{"mmc.multicomp"} object from the sufficient statistics
   for a one-way design.  The object must be explicitly plotted.
}
\usage{
simint.mean(y, n, ybar, s, alpha=.05,  ## R
            ylabel="ylabel", focus.name="focus.factor", plot=FALSE,
            lmat, labels=NULL,
            method="Tukey",
            bounds="both",
            df=sum(n) - length(n),
            sigmahat=(sum((n-1)*s^2) / df)^.5,
            contrasts, ..., group=y)

simint.mmc.mean(y, n, ybar, s, alpha=.05,  ## R
                ylabel="ylabel", focus.name="focus.factor", plot=FALSE,
                lmat, labels=NULL,
                method="Tukey",
                bounds="both",
                df=sum(n) - length(n),
                sigmahat=(sum((n-1)*s^2) / df)^.5,
                estimate.sign=1,
                order.contrasts=TRUE, ..., group=y)

multicomp.mean(group, n, ybar, s, alpha=.05,  ## S-Plus
               ylabel="ylabel", focus.name="focus.factor", plot=FALSE,
               lmat, labels=NULL, ...,
               df=sum(n) - length(n),
               sigmahat=(sum((n-1)*s^2) / df)^.5)

multicomp.mmc.mean(group, n, ybar, s, ylabel, focus.name,  ## S-Plus
                   lmat,
                   ...,
                   comparisons="mca",
                   lmat.rows=seq(length=length(ybar)),
                   ry,
                   plot=TRUE,
                   crit.point,
                   iso.name=TRUE,
                   estimate.sign=1,
                   x.offset=0,
                   order.contrasts=TRUE,
                   method="tukey",
                   df=sum(n)-length(n),
                   sigmahat=(sum((n-1)*s^2)/df)^.5)
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group, y}{character vector of levels}
  \item{n}{numeric vector of sample sizes}
  \item{ybar}{vector of group means }
  \item{s}{vector of group standard deviations}
  \item{alpha}{Significance levels of test}
  \item{ylabel}{name of response variable }
  \item{focus.name}{name of factor}
  \item{plot}{logical.  Should the \code{"mmc.multicomp"} object be
    automatically plotted?  ignored in R.}
  \item{lmat}{\code{lmat} from \code{multicomp} in S-Plus or
    \code{t(cmatrix)}
  from \code{simint} in R.}
  \item{labels}{\code{labels} argument for \code{multicomp} in S-Plus.
    Not used in R.}
  \item{method}{method for critical point calculation.  This corresponds
    to \code{method} in S-Plus \code{multicomp} and to \code{type}
    in R \code{simint}}
  \item{bounds}{type of intervals to compute.  This is the
    \code{"bounds"} argument to \code{multicomp} and the
    \code{alternative} argument to \code{simint}.  Values are: the
    default \code{"both"} for two-sided intervals; \code{"lower"} for
    intervals with infinite upper bounds; and, \code{"upper"} for
    intervals with infinite lower bounds.  In R, the S-Plus values are
    translated to \code{"two.sided"}, \code{"greater"}, and
    \code{"less"}.  Or the user can enter the values \code{"two.sided"},
    \code{"greater"}, and \code{"less"}.}
  \item{df}{scalar, residual degrees of freedom}
  \item{sigmahat}{\code{sqrt(MSE)} from the ANOVA table}
  \item{contrasts}{logical, argument in R to \code{contr.Dunnett}
    when \code{method="Dunnett"}. }
  \item{\dots}{other arguments}
  \item{comparisons}{argument to S-Plus \code{multicomp} only.}
  \item{estimate.sign, order.contrasts, lmat.rows}{See \code{lmat.rows} in
    \code{\link{multicomp.mmc}} or \code{\link{simint.mmc}}.}
  \item{ry}{See argument \code{ry.mmc} in \code{\link{plot.mmc.multicomp}}.}
  \item{crit.point}{See argument \code{crit.point} in S-Plus
    \code{\link[splus]{multicomp}}.  The equivalent is not in \code{simint}.}
  \item{iso.name, x.offset}{See \code{\link{plot.mmc.multicomp}}.}
}
\value{

  \code{simint.mmc.mean} and \code{multicomp.mmc.mean} return a
  "mmc.multicomp" object.

  \code{simint.mean} returns a "hmtest" object.
  
  \code{multicomp.mean} returns a "multicomp" object.

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
}
\seealso{\code{\link{simint.mmc}}, \code{\link{multicomp.mmc}}}
\examples{
## This example is from Hsu and Peruggia

pulmonary <- read.table(hh("datasets/pulmonary.dat"), header=TRUE,
                        row.names="group")
pulmonary

anova.mean(row.names(pulmonary),
           pulmonary$n,
           pulmonary$ybar,
           pulmonary$s,
           ylabel="pulmonary")


## simint or multicomp object
pulmonary.mca <-
if.R(r=
simint.mean(row.names(pulmonary),
            pulmonary$n,
            pulmonary$ybar,
            pulmonary$s,
            ylabel="pulmonary",
            focus="smoker")
,s=
multicomp.mean(row.names(pulmonary),
               pulmonary$n,
               pulmonary$ybar,
               pulmonary$s,
               ylabel="pulmonary",
               focus="smoker")
)

pulmonary.mca
## lexicographic ordering of contrasts, some positive and some negative
plot(pulmonary.mca)



pulm.lmat <- cbind("npnl-mh"=c( 1, 1, 1, 1,-2,-2), ## not.much vs lots
                   "n-pnl"  =c( 3,-1,-1,-1, 0, 0), ## none vs light 
                   "p-nl"   =c( 0, 2,-1,-1, 0, 0), ## {} arbitrary 2 df
                   "n-l"    =c( 0, 0, 1,-1, 0, 0), ## {} for 3 types of light
                   "m-h"    =c( 0, 0, 0, 0, 1,-1)) ## moderate vs heavy
dimnames(pulm.lmat)[[1]] <- row.names(pulmonary)
pulm.lmat


## mmc.multicomp object
pulmonary.mmc <-
if.R(r=
simint.mmc.mean(row.names(pulmonary),
                pulmonary$n,
                pulmonary$ybar,
                pulmonary$s,
                ylabel="pulmonary",
                focus="smoker",
                lmat=pulm.lmat)
,s=
multicomp.mmc.mean(row.names(pulmonary),
                   pulmonary$n,
                   pulmonary$ybar,
                   pulmonary$s,
                   ylabel="pulmonary",
                   focus="smoker",
                   lmat=pulm.lmat,
                   plot=FALSE)
)


gray <- if.R(r="gray", s=16)
red  <- if.R(r="red",  s=8)
blue <- if.R(r="blue", s=6)

old.par <- if.R(s=par(mar=c(5,4,4,4)+.1),
                r=par(mar=c(15,4,4,4)+.1))

## pairwise comparisons
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=FALSE,
     col.mca.signif=red, col.iso=16)

## tiebreaker plot, with contrasts ordered to match MMC plot,
## with all contrasts forced positive, and with names also reversed.
if.R(r={
pulmonary.xlim <- par()$usr[1:2]
plot(pulmonary.mmc$mca, xlim=pulmonary.xlim, xaxs="i", main="", xlab="")
},s={
plot(pulmonary.mmc$mca, col.signif=red, lty.signif=1, xlabel.print=FALSE,
     xaxs="d",  plt=par()$plt+c(0,0,-.25,.05), xrange.include=c(-1, 1.2))
})

## orthogonal contrasts
plot(pulmonary.mmc, print.lmat=TRUE, col.lmat.signif=blue, col.iso=16)

## pairwise and orthogonal contrasts on the same plot
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=TRUE,
     col.mca.signif=red, col.lmat.signif=blue, col.iso=16,
     lty.lmat.not.signif=2)

par(old.par)

}
\keyword{hplot }
\keyword{htest }
