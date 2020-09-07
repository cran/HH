\name{lm.case}
\alias{case}
\alias{case.lm}
%% \alias{lm.case}  %% leaving this in case someone searches on the older name
\alias{plot.case}
\alias{panel.case}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ case statistics for regression analysis}
\description{
  Case statistics for regression analysis.
  \code{case.lm} calculates the statistics.
  \code{plot.case} plots the cases, one statistic per panel, and
  illustrates and flags all observations for which the standard
  thresholds are exceeded.  \code{plot.case} returns an object with
  class \code{c("trellis.case", "trellis")}
  containing the plot and the row.names of the
  flagged observations.  The object is printed by a method which
  displays the set of graphs and prints the list of flagged cases.
  \code{panel.case} is a panel function for \code{plot.case}.
}
\usage{
case(fit, ...)
\method{case}{lm}(fit, lms = summary.lm(fit), lmi = lm.influence(fit), ...)

\method{plot}{case}(x, fit,
          which=c("stu.res","si","h","cook","dffits",
            dimnames(x)[[2]][-(1:8)]),  ##DFBETAS
          between.in=list(y=4, x=9),
          cex.threshold=1.2,
          main.in=list(
            paste(deparse(fit$call), collapse=""),
            cex=main.cex),
          sigma.in=summary.lm(fit)$sigma,
          p.in=summary.lm(fit)$df[1]-1,
          main.cex=NULL,
          ...)

panel.case(x, y, subscripts, rownames, group.names,
           thresh, case.large,
           nn, pp, ss, cex.threshold,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%case.lm
  \item{fit}{\code{"lm"} object computed with \code{x=TRUE}}
  \item{lms}{\code{summary.lm(fit)}}
  \item{lmi}{\code{lm.influence(fit)}}


%% plot.case
\item{x}{In \code{plot.case}, the matrix output from \code{case.lm}
  containing case diagnostics on each observation in the original
  dataset.
  In \code{panel.case}, the x variable to be plotted}
\item{which}{In \code{plot.case}, the names of the columns of x
  that are to be graphed.}
\item{between.in}{\code{between} trellis/lattice argument.}
%% \item{oma}{In S-Plus, change \code{par()$oma} to make room for the
%%   threshold values.  A warning is printed when  \code{par()$oma}
%%   is changed as the delayed printing of trellis objects implies we can't
%%   return it to the original value automatically.
%%   In R, this argument is ignored.  Instead, we use the
%%   \code{par.settings} argument to \code{xyplot} inside \code{plot.case}.
%%   The \code{par.settings} becomes one component of the \code{"trellis"}
%%   object that is the value of \code{plot.case} and is therefore
%%   automatically applied every time the object is printed.}
\item{cex.threshold}{Multiplier for \code{cex} for the threshold values.}
\item{main.in}{\code{main} title for \code{xyplot}. The default main title
  displays the linear model formula from \code{fit}.}
\item{sigma.in}{standard error for the \code{fit}.}
\item{p.in}{The number of degrees of freedom associated with the fitted
  model.}
%% \item{obs.large}{Object name where the names of
%%   all observations for which the standard
%%   thresholds are exceeded will be stored.  The default name is
%%   \code{.lm.case.large}.}
%% \item{obs.large.env}{Environment in
%%   R (defaults to \code{globalenv()}) where \code{.lm.case.large} will be stored.}
\item{main.cex}{\code{cex} for main title.}
\item{\dots}{other arguments to \code{xyplot}}

%% panel.case
 \item{y}{the y variable to be plotted.}
 \item{thresh}{Named list of lists.  Each list contains the components
 threshold ($y$-locations where a reference line will be drawn),
 thresh.label (the right-axis labels for the reference lines), thresh.id
 (the bounds defining "Noteworthy Observations").}
 \item{case.large}{Named list of "Noteworthy Observations".}
 \item{nn}{Number of rows in original dataset.}
 \item{pp}{The number of degrees of freedom associated with the fitted model.}
 \item{ss}{Standard error for the \code{fit}.}
 \item{subscripts}{trellis/lattice argument, position in the reshaped
   dataset constructed by \code{plot.case} before calling \code{xyplot}.}
 \item{rownames}{row name in the original data.frame.}
 \item{group.names}{names of the individual statistics.}
 %% \item{par.settings}{Used in R as part of the call to \code{xyplot}.
 %%   Although this argument is not used in the panel function,
 %%   it is needed as a formal argument in S-Plus to absorb it out of \code{\dots}
 %%   and thereby prevent it from being forwarded to \code{points}.}
}
\details{
\code{lm.influence} is part of S-Plus and R
\code{case.lm} and \code{plot.case} are based on:
Section 4.3.3 "Influence of Individual Obervations
in Chambers and Hastie", \emph{Statistical Models in S}.



}
\value{
  \code{case.lm} returns a matrix, with one row for each observation
  in the original dataset.  The columns contain the diagnostic statistics:
 \code{e} (residuals),
 \code{h}* (hat diagonals),
 \code{si}* (deleted standard deviation),
 \code{sta.res} (standardized residuals),
 \code{stu.res}* (Studentized deleted resididuals),
 \code{dffit} (difference in fits, change in predicted y when
 observation i is deleted),
 \code{dffits}* (standardized difference in fits, standardized change
 in predicted y when observation i is deleted),
 \code{cook}* (Cook's distance),
 and DFBETAs* (standardized difference in regression coefficients when
 observation i is deleted, one for each column of the x-matrix,
 including the intercept).


 \code{plot.case} returns a \code{c("trellis.case", "trellis")} object
  containing the plot
  (including the starred columns by default) and also retains the
  row.names of the flagged observations in the
  \code{$panel.args.common$case.large}
  component.  The print method for the \code{c("trellis.case",
    "trellis")}
  object prints the graph and the list of flagged observations.

  \code{panel.case} is a panel function for \code{plot.case}.
}
\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course with Examples in R}.
Second Edition.
Springer-Verlag, New York.
\url{https://www.springer.com/us/book/9781493921218}
 }
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link[stats]{lm.influence}}.
}
\examples{
data(kidney)

kidney2.lm <- lm(clearance ~ concent + age + weight + concent*age,
                 data=kidney,
                 na.action=na.exclude)  ## recommended

kidney2.case <- case(kidney2.lm)

## this picture looks much better in portrait, specification is device dependent

plot(kidney2.case, kidney2.lm, par.strip.text=list(cex=.9),
     layout=c(2,3))
}
\keyword{hplot}
\keyword{regression}
