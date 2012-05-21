\name{likert}
\alias{likert}
\alias{plot.likert}
\alias{plot.likert.default}
\alias{plot.likert.array}
\alias{plot.likert.list}
\alias{plot.likert.table}
\alias{plot.likert.ftable}
\alias{plot.likert.structable}
%%\alias{plot.likert.numeric}
\alias{plot.likert.data.frame}
\alias{xscale.components.top.HH}
\alias{yscale.components.right.HH}
\alias{floating}
\alias{pyramid}
\alias{sliding}
\alias{semantic}
\alias{differential}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Diverging stacked barcharts for Likert, semantic
differential, rating scale data, and population pyramids.}
\description{
Constructs and plots diverging stacked barcharts for Likert, semantic
differential, rating scale data, and population pyramids.
}
\usage{
likert(x, ..., xName = deparse(substitute(x)))
\method{plot}{likert}(x, ...)
\method{plot.likert}{default}(x,
            positive.order=FALSE,
            ylab=names(dimnames(x)[1]),
            xlab=if (as.percent != FALSE) "Percent" else "Count",
            main=xName,
            BrewerPaletteName="RdBu",
            ## These are the diverging palettes in RColorBrewer
            ## c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr",
            ## "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
            reference.line.col="gray65",
            middle.color="gray90",  ## "#F7F7F7" is the RColorBrewer default for middle.col in the RdBu scheme
            col.strip.background="gray97",
            col=likertColor(attr(x, "nlevels"),
              ReferenceZero, BrewerPaletteName, middle.color),
            as.percent=FALSE,
            par.settings.in=NULL,
            horizontal=TRUE,
            ReferenceZero=NULL,
            ...,
            key.border.white=TRUE,
            xName=deparse(substitute(x)),
            rightAxisLabels=rowSums(abs(x)),
            rightAxis=!missing(rightAxisLabels),
            ylab.right=if (rightAxis) "Row Count Totals" else NULL,
            panel=panel.barchart,
            xscale.components=xscale.components.top.HH,
            yscale.components=yscale.components.right.HH,
            xlimEqualLeftRight=FALSE,
            xTickLabelsPositive=TRUE,
            reverse=FALSE)

\method{plot.likert}{array}(x,
            condlevelsName=paste(names(dimnames(x))[-(1:2)], collapse="."),
            xName=deparse(substitute(x)),
            main=paste("layers of", xName, "by", condlevelsName),
            layout=if (horizontal) c(1, length(dim(x))-2) else c(length(dim(x))-2, 1),
            positive.order=FALSE,
            strip=TRUE,
            strip.left=TRUE,
            strip.left.values=names(x.pl), ## constructed from dimnames(x)[-(1:2)]
            strip.values=names(x.pl),
            strip.left.par=list(cex=1, lines=1),
            horizontal=TRUE,
            ...,
            resize.height=c("nrow","rowSums"),
            resize.width=1)

\method{plot.likert}{list}(x,  ## named list of matrices or 2D tables, ftables, structables
            condlevelsName="ListNames",
            xName=deparse(substitute(x)),
            main=paste("List items of", xName, "by", condlevelsName),
            layout=if (horizontal) c(1, length(x)) else c(length(x), 1),
            positive.order=FALSE,
            strip=!horizontal,
            strip.left=horizontal,
            strip.left.values=names(x),
            strip.values=names(x),
            strip.par=list(cex=1, lines=1),
            strip.left.par=list(cex=1, lines=1),
            horizontal=TRUE,
            ...,
            rightAxisLabels=sapply(x, function(x) rowSums(abs(x)), simplify = FALSE),
            rightAxis=!missing(rightAxisLabels),
            resize.height.tuning=-.5,
            resize.height=if (missing(layout)) {
              c("nrow","rowSums")
            } else {
              rep(1, layout[2])
            },
            resize.width=if (missing(layout)) {1 } else {
              rep(1, layout[1])
            },
            box.ratio=if (
              length(resize.height)==1 &&
              resize.height == "rowSums") 1000 else 2,
            xscale.components=xscale.components.top.HH,
            yscale.components=yscale.components.right.HH)

\method{plot.likert}{table}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{ftable}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{structable}(x, ..., xName=deparse(substitute(x)))
%%\method{plot.likert}{numeric}(x, ..., xName=deparse(substitute(x)))
\method{plot.likert}{data.frame}(x, ..., xName=deparse(substitute(x)))


xscale.components.top.HH(...)
yscale.components.right.HH(...)
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
    the right at the top of a panel of horizontal bars or at the left of
    a panel of vertical bars.
    \code{positive.order} is frequently set to \code{TRUE} for lists.}
  \item{as.percent}{When \code{as.percent==TRUE} or
    \code{as.percent=="noRightAxis"}, then the values in each row are rescaled
    to row percents.
    When \code{as.percent==TRUE} the original row totals are used as
    \code{rightAxisLabels},
    \code{rightAxis} is set to \code{TRUE}, the \code{ylab.right} is by
    default set to \code{"Row Count Totals"} (the user can change its value in
    the calling sequence).   When \code{as.percent=="noRightAxis"}, then
    \code{rightAxis} will be set to \code{FALSE}.
  }
  \item{par.settings.in}{This is a placeholder that lets the user
    specify some lattice \code{par.settings} and still retain the
    ones that are prespecified in the \code{plot.likert.default}.}
  \item{ReferenceZero}{Numeric scalar or \code{NULL}.  The position in
    the range \code{seq(0, attr(x, "nlevels")+.5, .5)} where the
    reference line at 0 will be placed.  \code{attr(x, "nlevels")} is the
    number of columns of the original argument \code{x}, \emph{before} it
    has been coerced to a \code{"likert"} object.  The default
    \code{NULL} corresponds to the middle level if there are an odd
    number of levels, and to half-way between the two middle levels if
    there are an even number of levels.  This argument is used when the
    number of positive levels and the number of negative levels are not
    the same.  For example, with 4 levels
    \code{c("Disagee", "Neutral", "Weak Agree", "Strong Agree")}, the
    argument would be specified \code{ReferenceZero=2} indicating that
    the graphical split would be in the middle of the second group with
    label \code{"Neutral"}.}
  \item{ylab, xlab, ylab.right, main}{Standard \code{lattice} graph
    labels in \code{\link[lattice]{barchart}}.}
  \item{BrewerPaletteName}{\code{\link[RColorBrewer]{RColorBrewer}}
    palette names. We default to the diverging palette \code{RdBu}.
    Diverging palettes are usually appropriate for two-directional
    scales (Agree--Disagree).  Sequential palettes are often appropriate
    for one-directional scales (Age Ranges).  Qualitative palettes are
    usually not appropriate for likert plots.  For even more control,
    the user can specify an arbitrary color set using the \code{col} argument.
  }
  \item{col}{Vector of color names for the levels of the agreement factor.
    Although the colors can be specified explicitly in the
    \code{col} argument, usually specifying one of the diverging
    palettes from \code{\link[RColorBrewer]{RColorBrewer}}
    in the argument \code{BrewerPaletteName} will be appropriate.
    For less intense colors, you can use the middle colors from a larger
    set of colors; e.g., \code{col=brewer.pal(6, "RdBu")[2:5]}.  See the last
    \code{AudiencePercent} example below for this usage.  Or you can use
    an arbitrary vector of color names, for example,
    \code{col=c('red','blue','#4AB3F2')}.  See \code{\link[grDevices]{colors}} for
    more details.
  }
  \item{reference.line.col}{Color for reference line at zero.}
  \item{middle.color}{Darker middle color than the default
    \code{"#F7F7F7"} in the \code{RdBu} scheme.}
  \item{col.strip.background}{Background color for the strip labels.}
  \item{key.border.white}{Logical. If \code{TRUE}, then place a white
    border around the \code{rect} in the key, else use the \code{col} of
    the rect itself.}
  \item{horizontal}{Logical, with default \code{TRUE} indicating
    horizontal bars, will be passed to the
    \code{barchart} function by the \code{plot.likert} method.
    In addition, it interchanges the meaning of \code{resize.height} and
    \code{resize.width} arguments
    to the \code{likert} functions applied to arrays and lists.}
  \item{\dots}{other arguments. These will be passed to the \code{barchart}
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
  \item{strip.left, strip}{Logical. The default \code{strip.left=TRUE}
    places the strip
    labels on the left of each panel as in the
    first professional challenges example.
    The alternative \code{strip.left=FALSE} puts
    the strip labels on the top of each panel, the traditional
    lattice strip label position.}
  \item{condlevelsName, strip.left.values, strip.values,
    strip.par, strip.left.par, layout}{Arguments which will be passed to
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
  \item{resize.height}{Either character scalar or numeric vector.
    If \code{"nrow"}, then the panels heights are proportional to the number of
    bars in each panel.  If \code{"rowSums"} and there is exactly one bar
    per panel,
    then the panels heights are proportional to the total count in each
    bar, and see the discussion of the \code{box.ratio} argument.
    If a numeric vector, the panel heights are proportional to the numbers
    in the argument.}
  \item{resize.width}{Numeric vector.  The panel widths are proportional to the numbers
    in the argument.}
  \item{box.ratio}{If there are more than one bar in any panel, then
    this defaults to the \code{trellis} standard value of 2.
    If there is exactly one bar in a panel, then the value is 1000, with
    the intent to minimize the white space in the panel.  In this way,
    when \code{as.percent==TRUE}, the
    bar total area is the count and the bar widths are all equal at
    100\%.  See the example below.}
  \item{panel}{panel function eventually to be used by \code{barchart}.
    %The \code{panel.likert} function calls the regular
    %\code{panel.barchart} and also handles the placement of the right axis
    %labels when needed.
  }
  \item{xscale.components, yscale.components}{See
    \code{\link[lattice]{yscale.components.default}}.
    \code{xscale.components.top.HH} constructs the top x-axis
    labels, when needed, as the names of the bottom x-axis labels.
    \code{yscale.components.right.HH} constructs the right y-axis
    labels, when needed, as the names of the left y-axis labels.  The
    names are placed automatically by the \code{plot.likert} methods based on
    the value of the arguments \code{as.percent}, \code{rightAxis},
    and \code{rightAxisLabels}.  By default, when \code{rightAxis != FALSE} the \code{layout.widths}
    are set to \code{list(ylab.right=5, right.padding=0)}.  Otherwise,
    those arguments are left at their default values.  They may be
    adjusted with an argument of the form
    \code{par.settings.in=list(layout.widths=list(ylab.right=5, right.padding=0))}.
    Similarly, spacing for the top labels can be adjusted with an
    argument of the form
    \code{par.settings.in=list(layout.heights=list(key.axis.padding=6))}.
  }
  \item{xlimEqualLeftRight}{Logical. The default is \code{FALSE}.  If
    \code{TRUE}, then the left and right x limits are set to negative
    and positive of the larger of the absolute value of the original x limits.}
  \item{xTickLabelsPositive}{Logical. The default is \code{TRUE}.
    If \code{TRUE}, then the tick labels on the negative side are
    displayed as positive values.}
  \item{reverse}{Logical. The default is \code{FALSE}.
    If \code{TRUE}, the rows of the input matrix are reversed.
    The default is to plot the rows from top-to-bottom
    for horizontal bars
    and from left-to-write for vertical bars.
    \code{reverse}, \code{positive.order}, and \code{horizontal} are
    independent.
    All eight combinations are possible. See the
    \code{Eight sequences and orientations} section
    in the example for all eight.
  }
 }
\details{
  The counts (or percentages) of respondents on each row who agree with
  the statement are shown to the right of the zero line; the counts (or
  percentages) who disagree are shown to the left. The counts (or
  percentages) for respondents who neither agree nor disagree are split
  down the middle and are shown in a neutral color. The neutral category
  is omitted when the scale has an even number of choices.  It is difficult to compare
  lengths without a common baseline. In this situation, we are primarily
  interested in the total count (or percent) to the right or left of the
  zero line; the breakdown into strongly or not is of lesser interest so
  that the primary comparisons do have a common baseline of zero. The
  rows within each panel are displayed in their original order by
  default. If the argument \code{positive.order=TRUE} is specified, the rows
  are ordered by the counts (or percentages) who agree. 

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
  Richard Heiberger and Naomi Robbins (2011),
  "Alternative to Charles Blow's Figure in \"Newt's War on Poor Children\"",
  Forbes OnLine, December 20, 2011.
 \url{http://www.forbes.com/sites/naomirobbins/2011/12/20/alternative-to-charles-blows-figure-in-newts-war-on-poor-children-2/}

 Naomi Robbins (2011), "Visualizing Data: Challenges to
 Presentation of Quality Graphics---and Solutions", Amstat News,
 September 2011, 28--30.
 \url{http://magazine.amstat.org/blog/2011/09/01/visualizingdata/}

Naomi B. Robbins and Richard M. Heiberger (2011).  Plotting Likert and
Other Rating Scales.  In JSM Proceedings, Section on Survey Research
Methods. Alexandria, VA: American Statistical Association, 1058--1066.
\url{https://www.amstat.org/membersonly/proceedings/2011/papers/300784_64164.pdf}

Luo, Amy and Tim Keyes (2005). "Second Set of Results in from the Career
Track Member Survey," Amstat News. Arlington, VA: American Statistical
Association.

}
\author{Richard M. Heiberger, with contributions from Naomi B. Robbins <naomi@nbr-graphs.com>.

Maintainer: Richard M. Heiberger <rmh@temple.edu> }

\note{
\describe{
  \item{Documentation note:}{
    Most of the plots drawn by \code{plot.likert} have a long
  left-axis tick label.  They therefore require a wider window
  than R's default of a nominal 7in \eqn{\times}{x} 7in window.  The
  comments with the examples suggest aesthetic window sizes.
}

\item{Technical note:}{
  There are three (almost) equivalent calling sequences for likert plots.
    \enumerate{
    \item
    \code{likert(x)  ## recommended}\cr
    \code{likert} is an alias for \code{plot.likert()}.

    \item
    \code{plot.likert(x)}\cr
    \code{plot.likert} is both a method of
    \code{plot} for \code{"likert"} objects, and a generic function in its own
    right.  There are methods of \code{plot.likert} for \code{"matrix"},
    \code{"array"}, \code{"table"}, and several other classes of input objects.
    
    \item
    \code{plot(as.likert(x))}\cr
    Both \code{likert} and \code{plot.likert} work by calling the
    \code{as.likert} function on their argument \code{x}.  Once \code{as.likert}
    has converted its argument to a \code{"likert"} object, the method
    dispatch technology for the generic \code{plot.likert} is in play.
    The user can make the explicit call \code{as.likert(x)} to see what
    a \code{"likert"} object looks like, but is very unlikely to want to
    look a second time.
}}}}

\examples{
require(grid)
require(lattice)
require(latticeExtra)
require(HH)

data(ProfChal)  ## List of named matrices.  See below for discussion of the dataset.
ProfChal[[2]]

## Count plot
likert(ProfChal[[2]],
       main='Is your job professionally challenging?',
       sub="This plot looks better in a 9in x 4in window.")

## Percent plot calculated automatically from Count data
likert(ProfChal[[2]], as.percent=TRUE,
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

likert(tmp2)
likert(tmp2, ReferenceZero=2.5, main="same as previous graph")
dimnames(tmp2)[[2]] <-
    c("Disagree", "Neutral", "Agree Weakly", "Agree Strongly")
likert(tmp2, ReferenceZero=2,
    main="Agreement Levels:\none disagree and two agree levels")
dimnames(tmp2)[[2]] <-
    c("Disagree Strongly", "Neutral", "Agree Weakly", "Agree Strongly")
likert(tmp2, ReferenceZero=2, col=likertColor(5, 3)[-2],
       main="Agreement Levels:\none strongly disagree and two agree levels")

lik2 <- as.likert(tmp2)
plot(lik2, main="default")
plot(lik2, xTickLabelsPositive=FALSE,
     main="xTickLabelsPositive=FALSE")
plot(lik2,                            xlimEqualLeftRight=TRUE,
     main="xlimEqualLeftRight=TRUE")
plot(lik2, xTickLabelsPositive=FALSE, xlimEqualLeftRight=TRUE,
     main="xTickLabelsPositive=FALSE, xlimEqualLeftRight=TRUE")


## likert(tmp3)
likert(tmp3, layout=c(1,4))
likert(tmp3, layout=c(2,2), resize.height=c(2,1), resize.width=c(3,4))

likert(tmp4, layout=c(3,2))
likert(tmp4, layout=c(3,2),
       rightAxis=TRUE, between=list(x=3))
likert(tmp4, layout=c(3,2),
       auto.key=list(border=TRUE, padding.text=4, height=.5))
likert(tmp4, layout=c(3,2), strip.left=FALSE)
likert(tmp4, layout=c(3,2), strip.left=FALSE, horizontal=FALSE)
likert(tmp4, layout=c(3,2), strip.left=FALSE,
       auto.key=list(border=TRUE, padding.text=4, height=.5),
       resize.height=1:2)


## plot.likert interprets vectors as single-row matrices.
## http://survey.cvent.com/blog/customer-insights-2/box-scores-are-not-just-for-baseball
Responses <- c(15, 13, 12, 25, 35)
names(Responses) <- c("Strongly Disagree", "Disagree", "No Opinion",
                      "Agree", "Strongly Agree")
likert(Responses, main="Retail-R-Us offers the best everyday prices.",
       sub="This plot looks better in a 9in x 2.6in window.")

likert(Responses, horizontal=FALSE,
       aspect=1.5,
       main="Retail-R-Us offers the best everyday prices.",
       sub="This plot looks better in a 7in x 3in window.")
##
## reverse=TRUE  is needed for a single-column key with
## horizontal=FALSE and with space="right"
likert(Responses, horizontal=FALSE,
       aspect=1.5,
       main="Retail-R-Us offers the best everyday prices.",
       auto.key=list(space="right", columns=1,
                     reverse=TRUE, padding.text=2),
       sub="This plot looks better in a 4in x 3in window.")

## plot.likert will plot a single-column matrix as if it were entirely
## "No Opinion".
likert(as.matrix(Responses),
       main="This is silly, but it doesn't give an error")


likert(ProfChal[[2]],
       horizontal=FALSE,
       scales=list(x=list(rot=90)),
       auto.key=list(columns=1, space="right", reverse=TRUE),
       main="Vertical diverging stacked barcharts are possible.\nThey usually do not look good when the bar labels are long.",
       sub="This plot looks less bad in a 8.5in x 7in window.")

\dontrun{
likert(ProfChal,
       horizontal=FALSE,
       scales=list(x=list(rot=90)),
       auto.key=list(columns=1, space="right", reverse=TRUE),
       main="Vertical diverging stacked barcharts are possible.\nThey usually do not look good when the bar labels are long.",
       sub="This plot looks less bad in a 8.5in x 7in window.")
}

## Diverging stacked bar charts have many uses in addition to plotting
## rating scales.  The AudiencePercent example, provided by Naomi
## B. Robbins, illustrates the age distribution of users of different
## brands of a product.  The original source for this example used
## multiple pie charts.

data(AudiencePercent)
AudiencePercent

likert(AudiencePercent,
      auto.key=list(between=1, between.columns=2),
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
\dontrun{
## Initially we use the "Blues" palette in the BrewerPaletteName argument.
likert(AudiencePercent,
       auto.key=list(between=1, between.columns=2),
       xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
       main="Target Audience",
       BrewerPaletteName="Blues",
       sub="This plot looks better in a 7in x 3.5in window.")
}

## The really light colors in the "Not run" example are too light.
## Therefore we use the col argument directly.  We chose to use an
## intermediate set of Blue colors selected from the "Blues" palette.
likert(AudiencePercent,
       auto.key=list(between=1, between.columns=2),
       xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
       main="Target Audience",
       col=brewer.pal(9, "Blues")[4:7],
       sub="This plot looks better in a 7in x 3.5in window.")

## Population Pyramid
data(USAge.table)
USA79 <- USAge.table[75:1, 2:1, "1979"]/1000000
PL <- likert(USA79,
             main="Population of United States 1979 (ages 0-74)",
             xlab="Count in Millions",
             ylab="Age",
             scales=list(
               y=list(
                 limits=c(0,77),
                 at=seq(1,76,5),
                 labels=seq(0,75,5),
                 tck=.5))
             )
PL
as.pyramidLikert(PL)

likert(USAge.table[75:1, 2:1, c("1939","1959","1979")]/1000000,
       main="Population of United States 1939,1959,1979 (ages 0-74)",
       sub="Look for the Baby Boom",
       xlab="Count in Millions",
       ylab="Age",
       scales=list(
         y=list(
           limits=c(0,77),
           at=seq(1,76,5),
           labels=seq(0,75,5),
           tck=.5)),
       strip.left=FALSE, strip=TRUE,
       layout=c(3,1), between=list(x=.5))


Pop <- rbind(a=c(3,2,4,9), b=c(6,10,12,10))
dimnames(Pop)[[2]] <- c("Very Low", "Low", "High", "Very High")
likert(as.listOfNamedMatrices(Pop),
            as.percent=TRUE,
            resize.height="rowSums",
            strip=FALSE,
            strip.left=FALSE,
            main="Area and Height are proportional to 'Row Count Totals'.\nWidth is exactly 100\%.")


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
print(ProfChal, minlength=6)

## 1. Plot counts with rows in each panel sorted by positive counts.
##
likert(ProfChal,
       positive.order=TRUE,
       main="This works, but needs more specified arguments to look good",
       sub="This looks better in a 10inx7in window")
##
## Strip labels on left (the default for plot.likert).
## positive.order=TRUE is the recommended (but not default) setting for
## lists because the rownames within each item of the list are usually
## different.
##
ProfChalCountsPlot <-
likert(ProfChal, box.width=unit(.4,"cm"),
       strip.left.par=list(cex=.7, lines=5),
       main=list("Is your job professionally challenging?",
                 x=unit(.65, "npc")),
       xlab="Counts",
       positive.order=TRUE,
       rightAxis=TRUE,  ## display Row Count Totals
       sub="This plot looks better in a 10in x 7in window.")
ProfChalCountsPlot

\dontrun{
## Strip labels on top (the default for most lattice plots).
## Change line spacing in the "Attitude" panel
names(ProfChal)[6] <- "Attitude toward Professional Recognition"
likert(ProfChal, box.width=unit(.3,"cm"),
    strip.left=FALSE, strip=TRUE,
    main=list("Is your job professionally challenging?", x=unit(.65, "npc")),
    xlab="Counts",
    positive.order=TRUE)
## Restore original line spacing
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## vertical bars
names(ProfChal)[6] <- "Prof Recog"
likert(ProfChal, horizontal=FALSE,
       positive.order=TRUE,
       scales=list(x=list(rot=45)),
       strip.par=list(cex=.7)) 
## Restore original line spacing
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## Eight sequences and orientations
names(ProfChal)[6] <- "Prof Recog"
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  reverse=FALSE, main="-h- default") ## default
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  reverse=TRUE,  main="-hr")
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, reverse=FALSE, main="---",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, reverse=TRUE,  main="--r",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  reverse=FALSE, main="ph-")
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  reverse=TRUE,  main="phr")
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, reverse=FALSE, main="p--",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, reverse=TRUE,  main="p-r",
       scales=list(x=list(rot=90)))

## Four sequences with the default value reverse=FALSE
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  main="-h default") ## default
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, main="--",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  main="ph")
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, main="p-",
       scales=list(x=list(rot=90)))

names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"
}

## 2. Plot percents with rows in each panel sorted by positive percents.
##    This is a different sequence than the counts.  Row Count Totals are
##    displayed on the right axis.
ProfChalPctPlot <-
likert(ProfChal,
    as.percent=TRUE,    ## implies display Row Count Totals
    box.width=unit(.4,"cm"),
    strip.left.par=list(cex=.7, lines=5),
    main=list("Is your job professionally challenging?",
              x=unit(.65, "npc")),
    xlab="Percent",
    positive.order=TRUE,
    sub="This plot looks better in a 10in x 7in window.")
ProfChalPctPlot


## 3. Putting both percents and counts on the same plot, both in
##    original sort order.

## Change name in the "Attitude" panel
names(ProfChal)[6] <- "Prof Recog"
 
LikertPercentCountColumns(ProfChal,
                          main="Is your job professionally challenging?",
                          sub="LikertPercentCountColumns test, 9x8 window")

## Restore original name
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"


## The ProfChal data is done again with explicit use of ResizeEtc
## in ?HH:::ResizeEtc
}
\keyword{hplot}



% ## 3. Putting both percents and counts on the same plot requires
% ##    coordination The easiest way is to order the original tables of
% ##    counts by the percents.

% ## Manually construct RowTotals and Tables of Row percents
% rowTotals <- sapply(ProfChal, rowSums, simplify=FALSE)
% rowTotals
% ProfChalPct <- as.listOfNamedMatrices(
%                    sapply(ProfChal, function(x) x / rowSums(x) * 100,
%                           simplify=FALSE))
% print(ProfChalPct, minlength=6)

% ## Find sort order for each matrix in the list
% PctOrders <- sapply(ProfChalPct,
%                     function(x) attr(as.likert(x), "positive.order"),
%                     simplify=FALSE)
% PctOrders

% ## Original data on Count scale, but ordered as for Percents
% ProfChalOrdered <- as.listOfNamedMatrices(
%                    mapply(FUN=function(L,O) L[order(O),,drop=FALSE],
%                           ProfChal, PctOrders,
%                           SIMPLIFY=FALSE))
% print(ProfChalOrdered, minlength=6)

% ## Counts plot in Pct order
% CountsPlot <-
% likert(ProfChalOrdered,
%        box.width=unit(.4,"cm"),
%        strip.left.par=list(cex=.7, lines=5),
%        main=list("Is your job professionally challenging?", x=unit(.65, "npc")),
%        xlab="Counts", ylab.right="Row Count Totals",
%        rightAxis=TRUE,
%        sub="This plot looks better in a 10in x 7in window.")
% CountsPlot

% ## Pct plot in Pct order
% PctPlot <-
% likert(ProfChalOrdered,
%        as.percent="noRightAxis", ## suppress right axis
%        box.width=unit(.4,"cm"),
%        strip.left.par=list(cex=.7, lines=5),
%        main=list("Is your job professionally challenging?", x=unit(.65, "npc")),
%        xlab="Percent",
%        sub="This plot looks better in a 10in x 7in window.")
% PctPlot


% \dontrun{
% ## These require functions not in the package because they
% ## are not yet stable.
% plotPctCounts(PctPlot, CountsPlot, plab=.47, f=.7)

% keepTrellis(CountsPlot,
%             Main=FALSE, LeftAxis=FALSE, LeftStrip=FALSE, BottomLegend=FALSE)
% }

% ## Remove main title, left axis tick labels, left strip, bottom legend
% ## from plot and keep the vertical spacing allocated to those items.
% CountsPlotRevised <- HH:::emptyMainLeftAxisLeftStripBottomLegend(CountsPlot) ## ugly but it works
% CountsPlotRevised

% ## Manual stretching of the graph window may be needed to get the
% ## alignment right.  For the standard 7x7 window, this aligns well
% ## (using either the pdf device or the windows device).
% ##
% PctPlotUpdate <-
% update(PctPlot,
%        sub="The 'position' arguments are set for a 7in x 7in window")
% print(PctPlotUpdate, position=c(0,0,.83,1), more=TRUE)
% print(CountsPlotRevised, position=c(.76,0,1,1), more=FALSE)

% ## This graph looks better in a wider window.
% ## Manual stretching of graph window may be needed to get the alignment right.
% ## The x position values (.8 and .75) are by trial and error.
% ## They depend on the current configuration of the plotting window.
% print(PctPlot, position=c(0,0,.8,1), more=TRUE)
% print(CountsPlotRevised, position=c(.75,0,1,1), more=FALSE)

