\name{AEdotplot.data.frame}  %% uppercase AE
\alias{AEdotplot.data.frame}
\alias{AEdotplot.AElogrelrisk}
\alias{AEdotplot.AEtable}
\alias{print.AEdotplot}
\alias{c.AEdotplot}
\alias{AElogrelrisk}
\alias{AEmatchSortorder}
\alias{update.AEdotplot}
\title{ AE (Adverse Events) dotplot of incidence and relative risk,
  support functions }
\description{
Support functions for the \code{\link{AEdotplot}}.
}

\usage{
  \method{AEdotplot}{data.frame}(xr, ...,
          conditionVariable=NULL,
          conditionName=deparse(substitute(xr)),
          useCondition=!is.null(conditionVariable),
          sub=list(conditionName, cex=.7))

  \method{AEdotplot}{AElogrelrisk}(xr,
          A.name=paste(levels(xr$RAND)[1], " (n=", xr$SN[1], ")", sep=""),
          B.name=paste(levels(xr$RAND)[2], " (n=", xr$SN[2], ")", sep=""),
          col.AB=c("red","blue"), pch.AB=c(16,17),
           main=if (sortbyRelativeRisk)
              list("Most Frequent On-Therapy Adverse Events Sorted by Relative Risk",
                   cex=1)
           else
              list("Most Frequent On-Therapy Adverse Events", cex=1),
          cex.AB.points=NULL, cex.AB.y.scale=.6, cex.x.scale=.6,
          panel.widths=c(.55, .22, .23),
          key.y=-.2, CI.percent=95,
          conditionName=deparse(substitute(xr)),
          sortbyRelativeRisk=TRUE,
          ...,
          sub=list(conditionName, cex=.7))

  \method{AEdotplot}{AEtable}(xr, ..., useCondition=TRUE,
                              sub="sub for AEsecond")

  \method{print}{AEdotplot}(x, ...,
      main=attr(x, "main"),
      sub=attr(x,"sub"),
      ae.key=attr(x, "ae.key"),
      panel.widths=attr(x,"panel.widths"),
      AEtable=TRUE)

  \method{c}{AEdotplot}(..., panel.widths=attr(aedp[[1]], "panel.widths"))

  AElogrelrisk(ae,
               A.name=levels(ae$RAND)[1],
               B.name=levels(ae$RAND)[2],
               crit.value=1.96,
               sortbyRelativeRisk=TRUE, ...)

  AEmatchSortorder(AEstandard,
                   AEsecond,
                   AEsecond.AEtable=attr(AEsecond, "AEtable"),
                   levels.order=
                      lapply(attr(AEstandard,"AEtable"),
                             function(AEsubtable) levels(AEsubtable$PREF)),
                   main.second=list(paste("Most Frequent On-Therapy Adverse Events",
                                          "Sorted to Match First Table"),
                                    cex=1))

  \method{update}{AEdotplot}(object, ...)
}

\arguments{
  \item{ae}{For \code{AElogrelrisk}, a data.frame containing at least the
    first 4 columns of \code{xr}.
  }
  \item{xr}{For the formula method documented in
    \code{\link{AEdotplot}}, a formula of the form\cr
    \code{AE ~ nAE/nTRT | OrgSys}, where the condition variable is
    optional.
    For the formula method only, the variable names are not restricted.

    For the other methods, \code{xr} is a \code{data.frame} containing the Adverse
  Event data in long format. It must have variables named\cr
  \code{RAND}:   treatment as randomized (factor with exactly two levels).\cr
  \code{PREF}:   adverse event symptom name (factor).\cr
  \code{SN}:     number of patients in treatment group.\cr
  \code{SAE}:    number of patients in each group for whom the event
  PREF was observed.

  If the \code{xr} object is a \code{AElogrelrisk} object, then it must
  also have variables\cr
  \code{PCT}: \code{SAE}/\code{SN} as a percent.\cr
  \code{relrisk}: Relative risk defined as \code{PCT} for the B
  treatment divided by \code{PCT} for the A treatment.\cr
  \code{logrelrisk}: natural logarithm of \code{relrisk}.\cr
  \code{ase.logrelrisk}: asymptotic standard error of \code{logrelrisk}.\cr
  \code{logrelriskCI.lower, logrelriskCI.upper}: confidence interval for
  \code{logrelrisk}.\cr
  \code{relriskCI.lower, relriskCI.upper}: back transform of the CI for
  the log relative risk into the relative risk scale.
  }

  \item{sortbyRelativeRisk}{logical.  If \code{TRUE}, then make the
    Adverse Events an ordered factor ordering by relative risk.
    If \code{FALSE}, then make the
    Adverse Events an ordered factor retaining the order of the input levels.}
  \item{conditionVariable}{Vector of same length as number of rows in
    \code{xr}, it may be one of the columns in \code{xr} in which case
    its full name in the form xr$varname must be used.  It will be used to
    split the \code{data.frame} into partitions.
    It may be used to partition the plot,
    for example by organ system or by gender.}
  \item{conditionName}{Character. Name to be used in \code{left.strip}.}
  \item{useCondition}{logical.  If \code{FALSE}, then a non-\code{NULL}
  \code{ConditionVariable} won't be used.}
  \item{x}{object to be printed.}
  \item{panel.widths}{Vector of three non-negative numerics that sum to 1.
     These are the widths of each of the three panels in the output plot.
     The left panel contains the AE names as y-tick labels and the Percent plot.
     The middle panel contains the Relative Risk plot.
     The right panel contains a table of the numerical values of number of patients
     for each treatment,
     number of adverse events for each treatment, and relative risk.
     Setting the third value to 0 suppresses the table of numerical
     values from the display.
   }
   \item{AEtable}{logical.  For the \code{print.AEdotplot} function.
   If \code{TRUE} (the default), display all three panels.  If
    \code{FALSE}, then display only the Percent and Relative Risk plots.}
   \item{main, sub}{Main title and subtitle for the combined plot
     in \code{AEdotplot}.}
   \item{main.second}{Main title for second plot whose sort order has
  been changed to match the first plot.}
  \item{A.name, B.name}{Names of treatment groups (in \code{x$RAND}).}
  \item{col.AB, pch.AB, cex.AB.points}{color, plotting character and
    character expansion for the individual points on the left plot.}
  \item{cex.AB.y.scale}{Character expansion for the left tick labels
    (the Adverse Effects names). }
  \item{cex.x.scale}{Character expansion for the x-axis tick labels.}
  \item{key.y}{Position of the key (legend) in the combined plot.  This
    is the \code{y} argument of the \code{key}.
    See the discussion of the \code{key} argument to \code{xyplot} in
    \code{\link[lattice]{xyplot}}}.
  \item{ae.key}{is a key as described in \code{\link[lattice]{xyplot}}.}

  \item{AEstandard, AEsecond, AEsecond.AEtable, levels.order}{Arguments
    that force the Adverse Events in the panels of \code{AEsecond} to
    have the same sort order \code{levels.order} of \code{PREF} as the
    panels of \code{AEstandard}. \code{AEstandard} and \code{AEsecond}
    are two \code{"AEdotplot"} objects with the same set of panels and
    the same Adverse Events in corresponding panels.
    \code{AEsecond.AEtable} is the \code{AEtable} object from
    \code{AEsecond}.  \code{levels.order} is the new order for
    \code{AEsecond}; normally the same order as in \code{AEprimary}.  }

  \item{crit.value}{Critical value used to compute confidence intervals
    on the log relative risk.  Defaults to 1.96.  User is responsible
    for specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

  \item{CI.percent}{Confidence percent associated with the
    \code{crit.value} Defaults to 95.  User is responsible for
    specifying both \code{crit.value} and \code{CI.percent}
    consistently.}

  \item{\dots}{For \code{AEdotplot} and \code{AEdotplot.data.frame},
    all the arguments listed in the
    calling sequence for \code{AEddotplot.AErelrisk}..
    For \code{c.AEdotplot}, one or more \code{"AEdotplot"} objects.
    For \code{print.AEdotplot}, the \code{\dots} arguments are ignored.
  }

  \item{object}{An \code{AEdotplot} object.  The \code{update} method
    updates the components of each of the constituent \code{trellis}
    objects.  It does not update the \code{"main"} and \code{"sub"}
    attributes (nor any other attribute) of the \code{AEdotplot} object.}

%\item{y, groups, lwd}{standard panel function arguments.}
%\item{lower, upper}{\code{xr$logrelriskCI.lower} and
%  \code{xr$logrelriskCI.upper} inside the panel functions.}
}

\details{
  The first panel is an ordinary dotplot of the percent of AE observed for each
  treatment by AE.

  The second panel shows relative risk of an event on the Treatment B arm
  (usually the active compound)
  relative to the Treatment A arm (usually the placebo), with 95\% confidence
  intervals for a \eqn{2\times2}{2x2} table.  Confidence intervals on the log
  relative risk are calculated using the asymptotic standard error
  formula given as Equation 3.18 in Agresti A., \emph{Categorical Data
  Analysis.} Wiley: New York, 1990.

  By default the \code{AEdotplot} function sorts the events by relative risk.
  To retain the sort order implied by the \code{levels} of the AE
  factor, specify the argument \code{sortbyRelativeRisk=FALSE}.
  To control the sort order, make the AE factor in the input dataset
  an \code{ordered} factor
  and specify the levels in the order you want.

  The third panel shows the numerical values of the number and percent
  of observed events on each arm and the relative risk.
  The display of third panel can be suppressed by specifying the
  \code{panel.widths} argument.
}

\value{
The primary interest is in the display of the plot.

The function returns
an \code{AEdotplot} object which is a list of three \code{trellis}
objects,
one for the the Percent plot, one for the Relative Risk plot, and one
for
the Text plot containing the table of input values.  The object has
attributes
\enumerate{
\item \code{main} and \code{sub} hold the main and subtitles.  Each must
be a list containing the text in the first component.
\item \code{ae.key} is a key as described in
\code{\link[lattice]{xyplot}}.
\item \code{n.events} is a vector containing the number of events in
each subpanel.
\item \code{panel.widths} is a vector of relative widths of the three components
of
the graph.  The numbers must sum to one.  Zero values are permitted.
The first width includes the left axis and the Percent plot.  The second
is the Relative Risk plot, and the third is the plot of the table
values.
\item \code{AEtable} is a table containing the data plotted on its row.
}
}

\seealso{\code{\link{AEdotplot}}}

\references{
Ohad Amit, Richard M. Heiberger, and Peter W. Lane. (2008)
``Graphical Approaches to the Analysis of Safety Data from Clinical Trials''.
\emph{Pharmaceutical Statistics},
\bold{7}, 1, 20--35.
% Published Online: 26 Feb 2007\cr
\url{http://www3.interscience.wiley.com/journal/114129388/abstract}
% \url{http://www3.interscience.wiley.com/cgi-bin/abstract/114129388/ABSTRACT}.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
\dontrun{
## variable names in the input data.frame aeanonym
## RAND   treatment as randomized
## PREF   adverse event symptom name
## SN     number of patients in treatment group
## SAE    number of patients  in each group for whom the event PREF was observed
## OrgSys Organ System
##
## Input sort order is PREF/RAND

data(aeanonym)
head(aeanonym)

## variable names are hard-wired in the program
## names(aeanonym) <- c("RAND", "PREF", "SAE", "SN", "OrgSys")


## Calculate log relative risk and confidence intervals (95% by default).
## AElogrelrisk sets the sort order for PREF to match the relative risk.
aeanonymr <- AElogrelrisk(aeanonym) ## PREF sorted by relative risk
head(aeanonymr)
class(aeanonymr$PREF)
levels(aeanonymr$PREF)

AEdotplot(aeanonym)
\dontrun{
AEdotplot(aeanonym, sort=FALSE)
}
AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys)

aefake <- rbind(cbind(aeanonym, group="ABC"), cbind(aeanonym, group="DEF"))
aefake$SAE[67:132] <- sample(aefake$SAE[67:132])
aefake$OrgSys.group <- with(aefake, interaction(OrgSys, group))

## fake 2
KEEP <- aefake$OrgSys \%in\% c("GI","Resp")
%% KEEP <- aefake$OrgSys %in% c("GI","Resp")
AEfakeGR <- AEdotplot(aefake[KEEP,], conditionVariable=aefake$OrgSys.group[KEEP],
            sub=list("ABC and DEF have different sort orders for PREF", cex=.7))
AEfakeGR ## ABC and DEF have different sort orders for PREF

AEfakeGR1 <- AEdotplot(aefake[KEEP & (1:132) <= 66,],
                       conditionVariable=aefake$OrgSys.group[KEEP & (1:132) <= 66])
AEfakeGR2 <- AEdotplot(aefake[KEEP & (1:132) >= 67,],
                       conditionVariable=aefake$OrgSys.group[KEEP & (1:132) >= 67])

AEfakeGR1
AEfakeGR2

AEfakeMatched <- AEmatchSortorder(AEfakeGR1, AEfakeGR2)
update(do.call(c, AEfakeMatched),
       main="ABC sorted by Relative Risk; DEF matches ABC order")
}
## Please see  ?AEdotplot  for examples using the formula method
##
## Many more examples are in demo("AEdotplotManyExamples")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ htest }
