\name{ae.dotplot}
\alias{ae.dotplot}  %% lowercase ae
\alias{AE.dotplot}  %% uppercase AE
\alias{panel.ae.leftplot}
\alias{panel.ae.rightplot}
\alias{panel.ae.dotplot}
\alias{logrelrisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ AE (Adverse Events) dotplot of incidence and relative risk }
\description{
A two-panel display of the most
frequently occurring AEs in the active arm of a clinical
study. The first panel displays their incidence by
treatment group, with different symbols for each
group. The second panel displays the relative risk
of an event on the active arm relative to the
placebo arm, with 95\% confidence intervals for a 2$\times$2 table.
The AEs are ordered by
relative risk so that events with the largest
increases in risk for the active treatment are
prominent at the top of the display.}
\usage{
ae.dotplot(xr,
           A.name = levels(xr$RAND)[1], B.name = levels(xr$RAND)[2],
           col.AB = c("red","blue"), pch.AB = c(16, 17),
           main.title = "Most Frequent On-Therapy Adverse Events Sorted by Relative Risk",
           main.cex = 1,
           cex.AB.points = NULL, cex.AB.y.scale = 0.6,
           position.left = c(0, 0, 0.7, 1), position.right = c(0.61, 0, 0.98, 1),
           key.y = -0.2, CI.percent=95)

logrelrisk(ae, A.name, B.name, crit.value=1.96)

panel.ae.leftplot(x, y, groups, col.AB, ...)

panel.ae.rightplot(x, y, ..., lwd=6, lower, upper)

panel.ae.dotplot(x, y, groups, ..., col.AB, pch.AB, lower, upper) ## R only
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ae}{data.frame containing the first 4 columns of \code{xr}
    described below.
  }
  \item{xr}{
  \code{RAND}:   treatment as randomized (factor).\\
  \code{PREF}:   adverse event symptom name (factor).\\
  \code{SN}:     number of patients in treatment group.\\
  \code{SAE}:    number of patients  in each group for whom the event PREF was observed.\\
  \code{PCT}: \code{SAE}/\code{SN} as a percent.\\
  \code{relrisk}: Relative risk defined as \code{PCT} for the B
  treatment divided by \code{PCT} for the A treatment.\\
  \code{logrelrisk}: natural logarithm of \code{relrisk}.\\
  \code{ase.logrelrisk}: asymptotic standard error of \code{logrelrisk}.\\
  \code{logrelriskCI.lower, logrelriskCI.upper}: confidence interval for
  \code{logrelrisk}.  \\
  \code{relriskCI.lower, relriskCI.upper}: back transform of the CI for
  the log relative risk into the relative risk scale.\\
  }
  \item{A.name, B.name}{Names of treatment groups (in \code{x$RAND}).}
  \item{col.AB, pch.AB, cex.AB.points}{color, plotting character and
    character expansion for the individual points on the left plot.}
  \item{cex.AB.y.scale}{Character expansion for the left tick labels
    (the symptom names). }
  \item{main.title, main.cex}{Main title and character expansion for the
  combined plot in \code{ae.dotplot}.}
  \item{position.left, position.right}{\code{position} of the left and
    right plots.  This argument is use in S-Plus only, not in R.
     See the discussion of \code{position} in
 \code{\link[lattice]{print.trellis}} in R,
 \code{\link[trellis]{print.trellis}} in S-Plus.  }
    
  \item{key.y}{Position of the key (legend) in the combined plot.  This
   is the \code{y} argument of the \code{key}.  See
   \code{\link[trellis]{key}} in S-Plus, and the discussion of the
   \code{key} argument to \code{xyplot} in \code{\link[lattice]{xyplot}} in
   R.}

  \item{crit.value}{Critical value used to compute confidence intervals
    on the log relative risk.  Defaults to 1.96.  User is responsible
    for specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

  \item{CI.percent}{Confidence percent associated with the
    \code{crit.value} Defaults to 95.  User is responsible for
    specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

\item{x, y, groups, lwd, \dots}{standard panel function arguments.}
\item{lower, upper} {\code{xr$logrelriskCI.lower} and
  \code{xr$logrelriskCI.upper} inside the panel functions.}
}

\details{
  The second panel shows relative risk of an event on the active arm
  (treatment B)
  relative to the placebo arm (treatment A), with 95\% confidence
  intervals for a 2$\times$2 table.  Confidence intervals on the log
  relative risk are calculated using the asymptotic standard error
  formula given as Equation 3.18 in Agresti A., Categorical Data
  Analysis. Wiley: New York, 1990.
}

\value{

  \code{logrelrisk} takes an input data.frame of the form \code{x}
  described in the argument list and returns a data.frame consisting of
  the input argument with additional columns as described in the
  argument \code{xr}.  The result column of symptom names \code{PREF} is
  an ordered factor, with the order specified by the relative risk.

 \code{ae.leftplot} returns a \code{"trellis"} object containing a
 horizontal dotplot of the percents against each of the symptom names.
 
 \code{ae.rightplot} returns a \code{"trellis"} object containing a
 horizontal plot on the log scale of the relative risk confidence
 intervals against each of the symptom names.

 \code{ae.dotplot} calls both \code{ae.leftplot} and \code{ae.rightplot}
 and combines their plots into a single display with a single set of
 left axis labels, a main title, and a key.  The value returned
 invisibly is a list of the full left trellis object and the right
 trellis obect with its left labels blanked out.  Printing the value
 will not usually be interesting as the main title and key are not
 included.
 It is better to call \code{ae.dotplot} directly, perhaps with a change
 in some of the positioning arguments.
 
  

  

}

\references{ Ohad Amit, Lane, Peter W., & Richard M. Heiberger (2007).
Graphical Approaches to the Analysis of Safety Data from Clinical
Trials. Pharmaceutical Statistics.
\url{http://www3.interscience.wiley.com/cgi-bin/abstract/114129388/ABSTRACT?CRETRY=1&SRETRY=0} }

\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## variable names in the input data.frame aeanonym
## RAND   treatment as randomized
## PREF   adverse event symptom name
## SN     number of patients in treatment group
## SAE    number of patients  in each group for whom the event PREF was observed
##
## Input sort order is PREF/RAND

aeanonym <- read.table(hh("datasets/aedotplot.dat"), header=TRUE, sep=",")
aeanonym[1:4,]

## Calculate log relative risk and confidence intervals (95% by default).
## logrelrisk sets the sort order for PREF to match the relative risk.
aeanonymr <- logrelrisk(aeanonym,
                        A.name=levels(aeanonym$RAND)[1],
                        B.name=levels(aeanonym$RAND)[2])
aeanonymr[1:4,]

## construct and print plot on current graphics device
ae.dotplot(aeanonymr,
           A.name="TREATMENT A (N=216)",
           B.name="TREATMENT B (N=431)")
## export.eps(h2("stdt/figure/aerelrisk.eps"))
## This looks great on screen and exports badly to eps.
## We recommend drawing this plot directly to the postscript device:
##
## trellis.device(postscript, color=TRUE, horizontal=TRUE,
##                colors=ps.colors.rgb[
##                  c("black", "blue", "red", "green",
##                    "yellow", "cyan","magenta","brown"),],
##                onefile=FALSE, print.it=FALSE,
##                file=h2("stdt/figure/aerelrisk.ps"))
## ae.dotplot(aeanonymr,
##            A.name="TREATMENT A (N=216)",
##            B.name="TREATMENT B (N=431)")
## dev.off()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ htest }
