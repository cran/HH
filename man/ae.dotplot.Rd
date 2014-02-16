\name{ae.dotplot}
\alias{ae.dotplot}  %% lowercase ae
\alias{AE.dotplot}  %% uppercase AE
\alias{ae.dotplot.long}
\alias{aeReshapeToLong}
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
placebo arm, with 95\% confidence intervals for a \eqn{2\times2}{2x2} table.
By default, the AEs are ordered by
relative risk so that events with the largest
increases in risk for the active treatment are
prominent at the top of the display.  See the Details section for
information
on changing the sort order.}
\usage{
ae.dotplot(ae, ...)

ae.dotplot.long(xr,
                A.name = levels(xr$RAND)[1], B.name = levels(xr$RAND)[2],
                col.AB = c("red","blue"), pch.AB = c(16, 17),
                main.title = paste("Most Frequent On-Therapy Adverse Events",
                                   "Sorted by Relative Risk"),
                main.cex = 1,
                cex.AB.points = NULL, cex.AB.y.scale = 0.6,
                position.left = c(0, 0, 0.7, 1), position.right = c(0.61, 0, 0.98, 1),
                key.y = -0.2, CI.percent=95)

logrelrisk(ae, A.name, B.name, crit.value=1.96)

panel.ae.leftplot(x, y, groups, col.AB, ...)

panel.ae.rightplot(x, y, ..., lwd=6, lower, upper, cex=.7)

panel.ae.dotplot(x, y, groups, ..., col.AB, pch.AB, lower, upper) ## R only

aeReshapeToLong(aewide)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ae}{For \code{ae.dotplot}, either a data.frame containing the Adverse
    Event data in long format as described by the detail for \code{xr}
    below, or a data.frame containing the Adverse event data in wide format as
    described by the detail for \code{aewide} below.
    For \code{logrelrisk}, a data.frame containing the first 4 columns of \code{xr}
    described below.
  }
  \item{\dots}{For \code{ae.dotplot}, all the arguments listed in the
  calling sequence for \code{ae.ddotplot.long} and possibly standard panel
  function arguments.

  For the other functions, just standard panel function arguments.
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
\item{aewide}{
  \code{Event}: adverse event symptom name (factor).\\
  \code{N.A, N.B}: number of patients in treatment groups A and B.\\
  \code{AE.A, AE.B}: number of patients in treatment groups A and B for
                     whom the event Event was observed.\\
  \code{PCT.A, PCT.B}: \code{AE.A}/\code{N.A} and  \code{AE.B}/\code{N.B} as a percent.\\
  \code{Relative.Risk}: Relative risk defined as \code{PCT.B}
   divided by \code{PCT.A}.\\
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
  \item{cex}{The character expansion for the points in the left and
   right plots.}
  \item{position.left, position.right}{\code{position} of the left and
    right plots.  This argument is use in S-Plus only, not in R.
     See the discussion of \code{position} in
%% R
#ifndef S-Plus
 \code{\link[lattice]{print.trellis}},
#endif
#ifdef S-Plus
 \code{\link[trellis]{print.trellis}}.
#endif
    }
  \item{key.y}{Position of the key (legend) in the combined plot.  This
   is the \code{y} argument of the \code{key}.
%% R
#ifndef S-Plus
   See the discussion of the \code{key} argument to \code{xyplot} in
   \code{\link[lattice]{xyplot}}.
#endif
#ifdef S-Plus
   See \code{\link[trellis]{key}}.
#endif
 }

  \item{crit.value}{Critical value used to compute confidence intervals
    on the log relative risk.  Defaults to 1.96.  User is responsible
    for specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

  \item{CI.percent}{Confidence percent associated with the
    \code{crit.value} Defaults to 95.  User is responsible for
    specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

\item{x, y, groups, lwd}{standard panel function arguments.}
\item{lower, upper}{\code{xr$logrelriskCI.lower} and
  \code{xr$logrelriskCI.upper} inside the panel functions.}
}

\details{
  The second panel shows relative risk of an event on the active arm
  (treatment B)
  relative to the placebo arm (treatment A), with 95\% confidence
  intervals for a \eqn{2\times2}{2x2} table.  Confidence intervals on the log
  relative risk are calculated using the asymptotic standard error
  formula given as Equation 3.18 in Agresti A., Categorical Data
  Analysis. Wiley: New York, 1990.

  By default the \code{ae.dotplot} function sorts the events by relative risk.
  To change the sort order, you must redefine the ordering of the
  ordered factor \code{PREF}.  See the examples below.

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
 trellis object with its left labels blanked out.  Printing the value
 will not usually be interesting as the main title and key are not
 included.
 It is better to call \code{ae.dotplot} directly, perhaps with a change
 in some of the positioning arguments.

}

\references{
Ohad Amit, Richard M. Heiberger, and Peter W. Lane. (2008)
``Graphical Approaches to the Analysis of Safety Data from Clinical Trials''.
\emph{Pharmaceutical Statistics},
\bold{7}, 1, 20--35.
% Published Online: 26 Feb 2007\\
\url{http://www3.interscience.wiley.com/journal/114129388/abstract}
% \url{http://www3.interscience.wiley.com/cgi-bin/abstract/114129388/ABSTRACT}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## variable names in the input data.frame aeanonym
## RAND   treatment as randomized
## PREF   adverse event symptom name
## SN     number of patients in treatment group
## SAE    number of patients  in each group for whom the event PREF was observed
##
## Input sort order is PREF/RAND

data(aeanonym)
head(aeanonym)

## Calculate log relative risk and confidence intervals (95% by default).
## logrelrisk sets the sort order for PREF to match the relative risk.
aeanonymr <- logrelrisk(aeanonym) ## sorts by relative risk
head(aeanonymr)

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

## To change the sort order, redefine the PREF factor.
## For this example, to plot alphabetically, use the statement
aeanonymr$PREF <- ordered(aeanonymr$PREF, levels=sort(levels(aeanonymr$PREF)))
ae.dotplot(aeanonymr,
           A.name="TREATMENT A (N=216)",
           B.name="TREATMENT B (N=431)",
           main.title="change the main title to reflect the new sort order")

\dontrun{
## to restore the order back to the default, use
relrisk <- aeanonymr[seq(1, nrow(aeanonymr), 2), "relrisk"]
PREF <- unique(aeanonymr$PREF)
aeanonymr$PREF <- ordered(aeanonymr$PREF, levels=PREF[order(relrisk)])
ae.dotplot(aeanonymr,
           A.name="TREATMENT A (N=216)",
           B.name="TREATMENT B (N=431)",
           main.title="back to the original sort order")

## smaller artifical example with the wide format
aewide <- data.frame(Event=letters[1:6],
                     N.A=c(50,50,50,50,50,50),
                     N.B=c(90,90,90,90,90,90),
                     AE.A=2*(1:6),
                     AE.B=1:6)
aewtol <- aeReshapeToLong(aewide)
xr <- logrelrisk(aewtol)
ae.dotplot(xr)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ htest }
