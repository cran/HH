\name{likert}
\alias{likert}
\alias{plot.likert}
\alias{plot.likert.default}
\alias{plot.likert.array}
\alias{plot.likert.list}
\alias{plot.likert.table}
\alias{plot.likert.ftable}
\alias{plot.likert.structable}
%% \alias{plot.likert.numeric}
\alias{plot.likert.data.frame}
\alias{panel.likert}
\alias{floating}
\alias{sliding}
\alias{semantic}
\alias{differential}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Diverging stacked barcharts for Likert, semantic
differential, and rating scale data.}
\description{
Constructs and plots diverging stacked barcharts for Likert, semantic
differential, and rating scale data.
}
\usage{
\method{plot}{likert}(x, ...)
\method{plot.likert}{default}(x,
            positive.order=FALSE,
            ylab=names(dimnames(x)[1]),
            xlab="Frequency",
            main=xName,
            BrewerPaletteName="RdBu",
            ## These are the diverging palettes in RColorBrewer
            ## c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr",
            ## "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
            reference.line.col="gray65",
            middle.col="gray90",  ## "#F7F7F7" is the RColorBrewer default for middle.col in the RdBu scheme
            col.strip.background="gray97",
            col=mybrewer(x, middle.col),
            as.percent=FALSE,
            ...,
            xName=deparse(substitute(x)),
            rightAxisLabels=rowSums(abs(x)),
            rightAxis=!missing(rightAxisLabels),
            ylab.right=if (rightAxis) "Row Totals" else NULL,
            panel=panel.likert)

\method{plot.likert}{array}(x,
            condlevelsName=paste(names(dimnames(x))[-(1:2)], collapse="."),
            xName=deparse(substitute(x)),
            main=paste("layers of", xName, "by", condlevelsName),
            layout=c(1, length(dim(x))-2),
            positive.order=FALSE,
            strip.left=TRUE,
            strip.left.values=rev(names(tt.pl)), ## constructed from dimnames(x)[-(1:2)]
            strip.left.par=list(cex=1, lines=1),
            ...)

\method{plot.likert}{list}(x,  ## named list of matrices or 2D tables, ftables, structables
            condlevelsName="ListNames",
            xName=deparse(substitute(x)),
            main=paste("List items of", xName, "by", condlevelsName),
            layout=c(1, length(x)),
            positive.order=FALSE,
            strip.left=TRUE,
            strip.left.values=rev(names(x)),
            strip.left.par=list(cex=1, lines=1),
            main.middle=.5,
            ...,
            rightAxisLabels=sapply(x, rowSums, simplify=FALSE),
            rightAxis=!missing(rightAxisLabels),
            resize.height.tuning=-.5)

\method{plot.likert}{table}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{ftable}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{structable}(x, ..., xName=deparse(substitute(x)))
%% \method{plot.likert}{numeric}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{data.frame}(x, ..., xName=deparse(substitute(x)))

panel.likert(..., rightAxisLabels, rightAxis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Any numeric object stored as a vector, matrix, array,
    data.frame, table, ftable,
    structable (as defined in the \code{vcd} package), or as a list of
    named two-dimensional objects.  This is the only required argument.
    See the Details section for restrictions on the form of data.frame,
    list, ftable, and structable arguments.
    }
  \item{positive.order}{If \code{FALSE}, the default value, the original
    order of the rows is retained.  This is necessary for arrays,
    because each panel has the same rownames.  If \code{TRUE}, rows are
    ordered within each panel with the row whose bar goes farthest to
    the right at the top.
    \code{positive.order} is frequently set to \code{TRUE} for lists.}
  \item{as.percent}{When \code{as.percent==TRUE} or
    \code{as.percent=="noRightAxis"}, then the values in each row are rescaled
    to row percents.
    When \code{as.percent==TRUE} the original row totals are used as
    \code{rightAxisLabels},
    \code{rightAxis} is set to \code{TRUE}, the \code{ylab.right} is by
    default set to \code{"row count totals"} (the user can change its value in
    the calling sequence).   When \code{as.percent=="noRightAxis"}, then
    \code{rightAxis} will be set to \code{FALSE}.
  }
  \item{ylab, xlab, ylab.right, main}{Standard \code{lattice} graph
    labels in \code{\link[lattice]{barchart}}.}
  \item{BrewerPaletteName}{\code{\link[RColorBrewer]{RColorBrewer}}
    palette names. We default to the diverging palette \code{RdBu}.
    If there are more levels than \code{RColorBrewer} normally handles, we
    automatically interpolate with
    \code{\link[grDevices]{colorRampPalette}}.
    Diverging palettes are usually appropriate for two-directional
    scales (Agree--Disagree).  Sequential palettes are often appropriate
    for one-directional scales (Age Ranges).  Qualitative palettes are
    usually not appropriate for likert plots.
  }
  \item{reference.line.col}{Color for reference line at zero.}
  \item{middle.col}{Darker middle color than the default
    \code{"#F7F7F7"} in the \code{RdBu} scheme.}
  \item{col.strip.background}{Background color for the strip labels.}
  \item{col}{Colors for the levels of the agreement factor.
    Although, the colors can be specified explicitly, usually specifying
    one of the diverging
    palettes from \code{link[RColorBrewer]{RColorBrewer}}
    in the argument \code{BrewerPaletteName} will be appropriate.
  }
  \item{\dots}{other arguments. These will be passed to the
    \code{barchart}
    function by the \code{plot.likert} method.
    Arguments to the \code{lattice} \code{auto.key=list()} argument
    (described in \code{\link[lattice]{barchart}}) will be used in the
    legend.  See the examples.}
  % \item{key.title,
  %   key.text,
  %   key.cex,
  %   key.border,
  %   key.height,
  %   key.space,
  %   key.columns,
  %   key.padding.text,
  %   key.size,
  %   key.between,
  %   key.between.columns,
  %   key.just,
  %   key.reverse
  % }{Arguments to the lattice \code{key}.
  %   See the discussion of \code{key} in \code{\link[lattice]{barchart}}.
  % }
  \item{strip.left}{Logical. The default \code{strip.left=TRUE}
    places the strip
    labels on the left of each panel as in the
    first professional challenges example.
    The alternative \code{strip.left=FALSE} puts
    the strip labels on the top of each panel, the traditional
    lattice strip label position.}
  \item{condlevelsName, strip.left.values,
    strip.left.par, layout, main.middle}{Arguments which will be passed to
    \code{\link{ResizeEtc}}.}
  \item{xName}{Name of the argument in its original environment.}
  \item{rightAxis}{logical.  Should right axis values be displayed?
    Defaults to \code{FALSE} unless
    \code{rightAxisLabels} are specified.}
  \item{rightAxisLabels}{Values to be displayed on the right axis.  The
    default values are the row totals.  These are sensible for
    tables of counts.  When the data is rescaled to percents by the
    \code{as.percent=TRUE} argument, then the \code{rightAxisLabels}
    are still defaulted to the row totals for the counts.
    We illustrate this usage in the \code{ProfChal} example.}
  \item{resize.height.tuning}{Tuning parameter used to adjust the space
    between bars as specified by the \code{resize.height} argument to
    the \code{\link{ResizeEtc}} function.}
  \item{panel}{panel function eventually to be used by \code{barchart}.
    The \code{panel.likert} function calls the regular
    \code{panel.barchart} and also handles the placement of the right axis
    labels when needed.}
}
\details{
  The counts (or percentages) of respondents on each row who agree with
  the statement are shown to the right of the zero line; the counts (or
  percentages) who disagree are shown to the left. The counts (or
  percentages) for respondents who neither agree nor disagree are split
  down the middle and are shown in a neutral color. The neutral category
  is omitted when the scale has an even number of choices. The
  categories within each sector are ordered by the counts (or
  percentages) who agree (by default). It is difficult to compare
  lengths without a common baseline. In this situation, we are primarily
  interested in the total count (or percent) to the right or left of the
  zero line; the breakdown into strongly or not is of lesser interest so
  that the primary comparisons do have a common baseline of zero.

  Diverging stacked barcharts are also called "two-directional stacked
  barcharts".  Some authors use the term "floating barcharts" for
  vertical diverging stacked barcharts and the term "sliding barcharts"
  for horizontal diverging stacked barcharts.

  All items in a list of named two-dimensional objects must have the
  same number of columns.  If the items have different column names, the
  column names of the last item in the list will be used in the key.  If
  the dimnames of the matrices are named, the names will be used in the
  plot.  It is possible to produce a likert plot with a list of objects
  with different numbers of columns, but not with the
  \code{plot.likert.list} method.  These must be done manually by using
  the \code{\link{ResizeEtc}} function on each of the individual likert
  plots.  The difficulty is that the legend is based on the last item in
  the list and will have the wrong number of values for some of the
  panels.
  
  A single data.frame \code{x} will be plotted as \code{data.matrix(x)};
  therefore factor columns will be converted to integers and character
  columns will become \code{NA} and will be plotted as if they had value
  0.  A data.frame with only numeric columns will work in a named list.
  A data.frame with factors or characters won't work in a named list.

  ftable and structable arguments \code{x} will be plotted as
  \code{as.table(x)}.  This changes the display sequence.
  Therefore the user will probably want to use \code{aperm} on the
  ftable or structable before using \code{plot.likert}.

}

\value{
 A \code{"trellis"} object containing the plot.  The plot will be
 automatically displayed unless the result is assigned to an object.
}
\seealso{\code{\link[lattice]{barchart}}, \code{\link{ResizeEtc}},
         \code{\link{as.likert}}, \code{\link{as.matrix.listOfNamedMatrices}}}
\references{
Naomi Robbins <naomi@nbr-graphs.com>, "Visualizing Data: Challenges to
 Presentation of Quality Graphics---and Solutions", Amstat News,
 September 2011, 28--30.

Naomi B. Robbins and Richard M. Heiberger (2011).  Plotting Likert and
Other Rating Scales.  In JSM Proceedings, Section on Survey Research
Methods. Alexandria, VA: American Statistical Association.

Luo, Amy and Tim Keyes (2005). "Second Set of Results in from the Career
Track Member Survey," Amstat News. Arlington, VA: American Statistical
Association.

}
\author{Richard M. Heiberger, with contributions from Naomi B. Robbins

Maintainer: Richard M. Heiberger <rmh@temple.edu> }

\note{Most of the plots drawn by \code{plot.likert} have a long
  left-axis tick label.  They therefore require a wider window
  than R's default of a nominal 7in \eqn{\times}{x} 7in window.  The
  comments with the examples suggest aesthetic window sizes.
}
\examples{
require(grid)
require(lattice)
require(latticeExtra)
require(HH)

data(ProfChal)  ## List of matrices.  See below for discussion of the dataset.
##
## ProfChal[[2]]
##
## The as.matrix.listOfNamedMatrices display is easier to read when the
## rownames are very long, as in this example.  Because the default
## print of the matrix repeats the rownames several times, with only a
## few columns of the data shown in each repetition, the actual matrix
## structure of the data values is obscured.
##
as.matrix.listOfNamedMatrices(ProfChal[2])

## Count plot
plot.likert(ProfChal[[2]],
            main='Is your job professionally challenging?',
            sub="This plot looks better in a 9in x 4in window.")

## Percent plot calculated automatically from Count data
plot.likert(ProfChal[[2]], as.percent=TRUE,
            main='Is your job professionally challenging?',
            sub="This plot looks better in a 9in x 4in window.")

## Examples with higher-dimensional arrays.
tmp2 <- array(1:12, dim=c(3,4), dimnames=list(B=LETTERS[3:5], C=letters[6:9]))
tmp3 <- array(1:24, dim=c(2,3,4),
              dimnames=list(A=letters[1:2], B=LETTERS[3:5], C=letters[6:9]))
tmp4 <- array(1:120, dim=5:2,
              dimnames=list(
                W=LETTERS[10:14],
                X=letters[6:9],
                Y=LETTERS[3:5],
                Z=letters[1:2]))

## positive.order=FALSE is the default.  With arrays
## the rownames within each item of an array are identical.
plot.likert(tmp2)
## plot.likert(tmp3)
plot.likert(tmp3, layout=c(1,4), auto.key=list(columns=3))
plot.likert(tmp4, layout=c(3,2), auto.key=list(columns=4))
plot.likert(tmp4, layout=c(3,2),
            auto.key=list(columns=4),
            rightAxis=TRUE, between=list(x=c(4,4)))
plot.likert(tmp4, layout=c(3,2),
            auto.key=list(columns=4, border=TRUE, padding.text=4, height=.5))
plot.likert(tmp4, layout=c(3,2), auto.key=list(columns=4), strip.left=FALSE)
plot.likert(tmp4, layout=c(3,2), strip.left=FALSE,
            auto.key=list(columns=4, border=TRUE, padding.text=4, height=.5))


## plot.likert interprets vectors as single-row matrices.
## http://survey.cvent.com/blog/customer-insights-2/box-scores-are-not-just-for-baseball
Responses <- c(15, 13, 12, 25, 35)
names(Responses) <- c("Strongly Disagree", "Disagree", "No Opinion",
                      "Agree", "Strongly Agree")
plot.likert(Responses, main="Retail-R-Us offers the best everyday prices.",
            sub="This plot looks better in a 9in x 2.6in window.")

## plot.likert will plot a single-column matrix as if it were entirely
## "No Opinion".
plot.likert(as.matrix(Responses),
            main="This is silly, but it doesn't give an error")


## Vertical diverging stacked barcharts are possible.
## They usually do not look good and are not recommended.
plot.likert(Responses, horizontal=FALSE,
            aspect=1.5,
            main="Retail-R-Us offers the best everyday prices.")
##
## reverse=TRUE  is needed for a single-column key with
## horizontal=FALSE and with space="right"
plot.likert(Responses, horizontal=FALSE,
            aspect=1.5,
            main="Retail-R-Us offers the best everyday prices.",
            auto.key=list(space="right", columns=1,
                          reverse=TRUE, padding.text=2),
            sub="This plot looks better in a 4in x 3in window.")
plot.likert(Responses, horizontal=FALSE,
            aspect=1.5,
            main="Retail-R-Us offers the best everyday prices.",
            auto.key=list(size=3, columns=5, between=.5,
	                  between.columns=2, just=.5),
            sub="This plot looks better in a 7in x 3in window.")

plot.likert(ProfChal[[2]],
            horizontal=FALSE,
            scales=list(x=list(rot=90)),
            auto.key=list(columns=1, space="right", reverse=TRUE),
            main="Vertical diverging stacked barcharts are possible.
They usually do not look good and are not recommended.",
            sub="This plot looks less bad in a 8.5in x 7in window.")


## Diverging stacked bar charts have many uses in addition to plotting
## rating scales.  The AudiencePercent example, provided by Naomi
## B. Robbins, illustrates the age distribution of users of different
## brands of a product.  The original source for this example used
## multiple pie charts.

data(AudiencePercent)
AudiencePercent

plot.likert(AudiencePercent,
            auto.key=list(columns=4, between=1, between.columns=2),
            xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
            main="Target Audience",
            sub="This plot looks better in a 7in x 3.5in window.")

## Color palettes from RColorBrewer can be named.
##
## Since age is always positive and increases in a single direction,
## this example uses colors from a sequential palette for the age
## groups.  In this example we do not use a diverging palette that is
## appropriate when groups are defined by a characteristic, such as
## strength of agreement or disagreement, that can increase in two directions.
## Initially we use the "Blues" palette in the BrewerPaletteName argument.
plot.likert(AudiencePercent,
            auto.key=list(columns=4, between=1, between.columns=2),
            xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
            main="Target Audience",
            BrewerPaletteName="Blues",
            sub="This plot looks better in a 7in x 3.5in window.")

## The really light colors in the previous example are too light.
## Therefore we use the col argument directly.  We chose to use an
## intermediate set of Blue colors selected from the "Blues" palette.
plot.likert(AudiencePercent,
            auto.key=list(columns=4, between=1, between.columns=2),
            xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
            main="Target Audience",
            col=brewer.pal(9, "Blues")[4:7],
            sub="This plot looks better in a 7in x 3.5in window.")


## Professional Challenges example.
##
## The data for this example is a list of related likert scales, with
## each item in the list consisting of differently named rows.  The data
## is from a questionnaire analyzed in a recent Amstat News article.
## The study population was partitioned in several ways.  Data from one
## of the partitions (Employment sector) was used in the first example
## in this help file.  The examples here show various options for
## displaying all partitions on the same plot.
##
data(ProfChal)
## ProfChal
as.matrix.listOfNamedMatrices(ProfChal, minlength=6)

## 1. Plot counts with rows in each panel sorted by positive counts.
##
## Strip labels on left (the default for plot.likert).
## positive.order=TRUE is the recommended (but not default) setting for
## lists because the rownames within each item of the list are usually
## different.
##
ProfChalCountsPlot <-
plot.likert(ProfChal, box.width=unit(.4,"cm"),
            strip.left.par=list(cex=.7, lines=5),
            main='Is your job professionally challenging?',
            main.middle=.65,
            xlab="Counts",
            positive.order=TRUE,
            rightAxis=TRUE,  ## display row count totals
            sub="This plot looks better in a 10in x 7in window.")
ProfChalCountsPlot

## Strip labels on top (the default for most lattice plots).
## Change line spacing
names(ProfChal)[6] <- "Attitude toward Professional Recognition"
plot.likert(ProfChal, box.width=unit(.3,"cm"),
            strip.left=FALSE,
            main='Is your job professionally challenging?',
            main.middle=.65,
            xlab="Counts",
            positive.order=TRUE)
## Restore original line spacing
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## 2. Plot percents with rows in each panel sorted by positive percents.
##    This is a different sequence than the counts.  Row Counts are
##    displayed on the right axis.
ProfChalPctPlot <-
plot.likert(ProfChal,
            as.percent=TRUE,    ## implies display Row Count Totals
            box.width=unit(.4,"cm"),
            strip.left.par=list(cex=.7, lines=5),
            main='Is your job professionally challenging?',
            main.middle=.65,
            positive.order=TRUE,
            sub="This plot looks better in a 10in x 7in window.")
ProfChalPctPlot



## 3. Putting both percents and counts on the same plot requires
##    coordination The easiest way is to order the original tables of
##    counts by the percents.

## Manually construct RowTotals and Tables of Row percents
rowTotals <- sapply(ProfChal, rowSums, simplify=FALSE)
rowTotals
ProfChalPct <- sapply(ProfChal, function(x) x / rowSums(x) * 100,
                      simplify=FALSE)
as.matrix.listOfNamedMatrices(ProfChalPct, minlength=6)

## Find sort order for each matrix in the list
PctOrders <- sapply(ProfChalPct,
                    function(x) attr(as.likert(x), "positive.order"),
                    simplify=FALSE)
PctOrders

## Original data on Count scale, but ordered as for Percents
ProfChalOrdered <- mapply(FUN=function(L,O) L[order(O),,drop=FALSE],
                          ProfChal, PctOrders,
                          SIMPLIFY=FALSE)
as.matrix.listOfNamedMatrices(ProfChalOrdered, minlength=6)

## Counts plot in Pct order
CountsPlot <-
plot.likert(ProfChalOrdered,
            box.width=unit(.4,"cm"),
            strip.left.par=list(cex=.7, lines=5),
            main='Is your job professionally challenging?',
            main.middle=.65,
            xlab="Counts", ylab.right="Row Count Totals",
            rightAxis=TRUE,
            sub="This plot looks better in a 10in x 7in window.")
CountsPlot

## Pct plot in Pct order
PctPlot <-
plot.likert(ProfChalOrdered,
            as.percent="noRightAxis", ## suppress right axis
            box.width=unit(.4,"cm"),
            strip.left.par=list(cex=.7, lines=5),
            main='Is your job professionally challenging?',
            main.middle=.65,
            xlab="Percent",
            sub="This plot looks better in a 10in x 7in window.")
PctPlot

## Remove main title, left axis tick labels, left strip, bottom legend
## from plot and keep the vertical spacing allocated to those items.
CountsPlotRevised <- emptyMainLeftAxisLeftStripBottomLegend(CountsPlot)
CountsPlotRevised

## Manual stretching of the graph window may be needed to get the
## alignment right.  For the standard 7x7 window, this aligns well
## (using either the pdf device or the windows device).
##
PctPlotUpdate <-
update(PctPlot,
       sub="The 'position' arguments are set for a 7in x 7in window")
print(PctPlotUpdate, position=c(0,0,.83,1), more=TRUE)
print(CountsPlotRevised, position=c(.76,0,1,1), more=FALSE)

## This graph looks better in a wider window.
## Manual stretching of graph window may be needed to get the alignment right.
## The x position values (.8 and .75) are by trial and error.
## They depend on the current configuration of the plotting window.
print(PctPlot, position=c(0,0,.8,1), more=TRUE)
print(CountsPlotRevised, position=c(.75,0,1,1), more=FALSE)

## The ProfChal data is done again with explicit use of ResizeEtc
## in ?HH:::ResizeEtc
}
\keyword{hplot}
