\name{AEdotplot}  %% uppercase AE
\alias{AEdotplot}
\alias{AEdotplot.formula}
\title{ AE (Adverse Events) dotplot of incidence and relative risk }
\description{
A three-panel display of the most
frequently occurring AEs in the active arm of a clinical
study. The first panel displays their incidence by
treatment group, with different symbols for each
group. The second panel displays the relative risk
of an event on the active arm relative to the
placebo arm, with 95\% confidence intervals for a \eqn{2\times2}{2x2} table.
By default, the AEs are ordered by
relative risk so that events with the largest
increases in risk for the active treatment are
prominent at the top of the display.
By setting the argument \code{sortbyRelativeRisk=FALSE}, the AEs retain
the order specified by the levels of the factor.
The third panel displays the numerical values of number of patients for
each treatment,
number of adverse events for each treatment, and relative risk.
The third panel can be suppressed by the \code{print} method.
}

\usage{
  AEdotplot(xr, ...)

  \method{AEdotplot}{formula}(xr, groups=NULL, data=NULL,
          sortbyRelativeRisk=TRUE,
          ...,
          sub=list(deparse(this.call[1:4],
                                width.cutoff=500), cex=.7))

}

\arguments{
  \item{xr}{For the formula method, a formula of the form
    \code{AE ~ nAE/nTRT | OrgSys}, where the condition variable is
    optional.
    For the formula method only, the variable names are not restricted.
    See \code{\link{AEdotplot.data.frame}} for the support methods.
  }

  \item{groups}{Variable containing the treatment levels.}

  \item{data}{\code{data.frame} containing at least four variables:
    containing the AE name, the treatment level, the number of observed
    AE in that treatment level, the number of
    patients in that treatment group. It may also contain a fifth variable
    containing a condition variable used to
    split the \code{data.frame} into partitions.
    It may be used to partition the plot,
    for example by organ system or by gender.
    The treatment factor must have exactly two levels.
  Each AE name must appear exactly once for each level of the treatment.}
  \item{sortbyRelativeRisk}{logical.  If \code{TRUE}, then make the
    Adverse Events an ordered factor ordering by relative risk.
    If \code{FALSE}, then make the
    Adverse Events an ordered factor retaining the order of the input levels.}

  \item{sub}{Subtitle for the plot.  The default value is the command
  that generates the plot.}

  \item{\dots}{Any of the arguments listed in the
    calling sequence for the methods documented in
    \code{\link{AEdotplot.data.frame}}.
  }
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
  \code{panel.widths} argument.  See the discussion of the
  \code{panel.widths} in \code{\link{AEdotplot.data.frame}}.
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

\seealso{\code{\link{AEdotplot.data.frame}}}


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
## formula method.  See ?AEdotplot.data.frame for other methods.
data(AEdata)
head(AEdata)

AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)

\dontrun{
%% AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT,
%%           data = AEdata[c(AEdata$OrgSys %in% c("GI","Resp")),])
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT,
          data = AEdata[c(AEdata$OrgSys \%in\% c("GI","Resp")),])

## test sortbyRelativeRisk=FALSE
ABCD.12345 <- AEdata[1:12,]
head(ABCD.12345)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups=TRT, data=ABCD.12345)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups=TRT, data=ABCD.12345, sort=FALSE)

## suppress third panel
tmp <- AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
print(tmp, AEtable=FALSE)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ htest }
