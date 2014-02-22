\name{likertMosaic}
\alias{likertMosaic}
\alias{likertMosaic.array}
\alias{likertMosaic.data.frame}
\alias{likertMosaic.default}
\alias{likertMosaic.formula}
\alias{likertMosaic.list}
\alias{likertMosaic.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diverging stacked barcharts for Likert, semantic
  differential, rating scale data, and population pyramids
  based on mosaic as the plotting style.

}
\description{
Constructs and plots diverging stacked barcharts for Likert, semantic
differential, rating scale data, and population pyramids,
.based on \code{mosaic} as the plotting style.
}
\usage{
likertMosaic(x, ...)

\method{likertMosaic}{formula}(x, data, ReferenceZero = NULL, spacing=NULL,
                   ..., between.y = c(1.2, 0.3))

\method{likertMosaic}{array}(x, ReferenceZero = NULL, col = NULL, main = NULL,
             ...,
             as.percent = FALSE, variable.width = NULL, positive.order = FALSE,
             Conditions = NULL,
             x.legend = list(text = list(dimnames(x)[[ndim]]),
                             columns = x.dim[ndim],
                             space = "bottom",
                             size = 2,
                             cex = 0.8,
                             between = 0.6,
                             rect= list(col = col, border = "white")),
             legend.y = 0.05,
             spacing = spacing_highlighting,
             split_vertical = c(TRUE, FALSE),
             margins = c(3, 2, 4, 22),
             keep_aspect = FALSE,
             rot_labels = c(0, 0, 90, 0),
             just_labels = c("center", "center", "center", "right"),
             labels = c(TRUE, TRUE, FALSE, TRUE),
             varnames = FALSE,
             zero_size = 0,
             gp = gpar(fill = col.extended, col = 0),
             colorFunction="diverge_hcl",
             colorFunctionOption="lighter")

\method{likertMosaic}{data.frame}(x, ...)

\method{likertMosaic}{default}(x, ...) ## most likely for a vector

\method{likertMosaic}{list}(x, ...)

\method{likertMosaic}{matrix}(x, ...,
 split_vertical = c(FALSE, TRUE),
 rot_labels = c(90, 0, 0, 0),
 just_labels = c("left", "center", "center", "right"),
 labels = c(TRUE, FALSE))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{For the formula method, a model formula.  Otherwise,
    any numeric object stored as a vector, matrix, array,
    data.frame, table, ftable,
    structable (as defined in the \code{vcd} package), or as a list of
    named two-dimensional objects.  This is the only required argument.
    See the Details section for restrictions on the form of data.frame,
    list, ftable, and structable arguments.
  }
  \item{data}{For the formula method, a \code{data.frame}.}
  \item{ReferenceZero}{Numeric scalar or \code{NULL}.  The position in
    the range\cr
    \code{seq(0, attr(x, "nlevels")+.5, .5)} where the
    reference line at 0 will be placed.  \code{attr(x, "nlevels")} is the
    number of columns of the original argument \code{x}, \emph{before} it
    has been coerced to a \code{"likert"} object.  The default
    \code{NULL} corresponds to the middle level if there are an odd
    number of levels, and to half-way between the two middle levels if
    there are an even number of levels.  This argument is used when the
    number of positive levels and the number of negative levels are not
    the same.  For example, with 4 levels\cr
    \code{c("Disagee", "Neutral", "Weak Agree", "Strong Agree")}, the
    argument would be specified \code{ReferenceZero=2} indicating that
    the graphical split would be in the middle of the second group with
    label \code{"Neutral"}.}
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
  }
  \item{variable.width}{
When \code{TRUE} and \code{as.percent==TRUE}, then the area of the bars
(percent along the length times the width) is proportional to the counts.
}
  \item{col}{
Colors for the bars.  With the default value \code{NULL}, the colors are
chosen from the default \code{\link[colorspace]{diverge_hcl}} diverging
palette.  Any color specification that R understands can be used here.
}
  \item{colorFunction, colorFunctionOption}{See \code{\link{likertColor}}.}
  \item{main}{
\code{main} title for the plot.
  }
  \item{\dots}{
Additional arguments, passed to the next method and possibly all the way
to \code{strucplot}.
}
  \item{Conditions}{
Factor used to divide the rows of the plot into sets of rows
corresponding to levels of Condition. In the formula method, the
conditions are the factors appearing after the \code{|} symbol.
}
  \item{between.y}{
vertical spacing between bars.  \code{between.y[1]} is used between levels
  of conditioning factors, and \code{between.y[2]} is used between bars within the
  same level of the conditioning factor.
}
  \item{x.legend}{
 Description of legend using the terminology and conventions of the
 \code{lattice} package.
}
  \item{legend.y}{
Adjust vertical location of legend.
}
  \item{spacing, split_vertical, margins, keep_aspect, rot_labels,
    just_labels, labels}{
    Please see \code{\link[vcd]{strucplot}} for details.
  }
  \item{varnames, zero_size, gp}{
    Please see \code{\link[vcd]{strucplot}} for details.
  }
}
\details{
  The counts (or percentages) of respondents on each row who agree with
  the statement are shown to the right of the zero line; the counts (or
  percentages) who disagree are shown to the left. The counts (or
  percentages) for respondents who neither agree nor disagree are split
  down the middle and are shown in a neutral color. The neutral category
  is omitted when the scale has an even number of choices.
  It is difficult to compare
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
Please see \code{\link[vcd]{strucplot}} for a description of the
returned object.
}
\references{
  Richard M. Heiberger, Naomi B. Robbins (2014).,
        "Design of Diverging Stacked Bar Charts for Likert Scales and Other Applications",
        Journal of Statistical Software, 57(5), 1--32,
        \url{http://www.jstatsoft.org/v57/i05/.}

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
\author{Richard M. Heiberger,
        with contributions from Naomi B. Robbins <naomi@nbr-graphs.com>.

Maintainer: Richard M. Heiberger <rmh@temple.edu> }
\note{
The functions described here are currently missing the following features:
\enumerate{
\item no axis ticks, number, nor axis label for the x axis
\item no zero reference line
\item no right-axis labels for Row Count Totals
\item no strip.left labels for grouping by Conditions
\item In Figure 8 and 9 (\code{HH/demo/likertMosaic-paper.r}),
      no control of the thickness of the bars
\item All bars are horizontal.
\item No borders on the overall plot nor on the panels in plots with
      grouping by Conditions
\item No control of \code{between=list(x=number)}
\item \code{cex} for labeling
\item border on empty boxes
\item I am using a lattice legend, not a native strucplot legend
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{likert}}, \code{\link[vcd]{mosaic}}

}
\examples{
## See file HH/demo/likertMosaic-paper.r for a complete set of examples.
\dontrun{
  require(vcd)
  data(ProfChal)
  likertMosaic(Question ~ . | Subtable, ProfChal,
               main="Is your job professionally challenging?")
  likertMosaic(Question ~ . | Subtable, ProfChal,
               main="Is your job professionally challenging?", as.percent=TRUE)
  likertMosaic(Question ~ . | Subtable, ProfChal,
               main="Is your job professionally challenging?", as.percent=TRUE,
               positive.order=TRUE)
  likertMosaic(Question ~ . | Subtable, ProfChal,
               main="Is your job professionally challenging?", as.percent=TRUE,
               variable.width=TRUE)

  EmpRows <- ProfChal$Subtable == "Employment sector"
  ProfChal2 <- ProfChal[EmpRows, 1:5]
  rownames(ProfChal2) <- substr(ProfChal[EmpRows, "Question"], 1, 5)

  likertMosaic(ProfChal2)
  likertMosaic(ProfChal2, main="Employment")
  likertMosaic(ProfChal2, main="Employment", ReferenceZero=0)
  likertMosaic(ProfChal2, main="Employment", ReferenceZero=3.5)
  likertMosaic(ProfChal2, main="Employment", ReferenceZero=4)
  likertMosaic(ProfChal2, main="Employment", ReferenceZero=6)
  likertMosaic(ProfChal2, main="Employment", positive.order=TRUE)
  likertMosaic(ProfChal2, main="Employment", variable.width=TRUE)

  likertMosaic(~ ., data.frame(ProfChal2), main="Employment", positive.order=TRUE)

  likertMosaic(~ ., data.frame(ProfChal2), main="Employment", variable.width=TRUE)
  likert(~ ., data.frame(ProfChal2), main="Employment", variable.width=TRUE)

  data(SFF8121)
  likertMosaic(aperm(SFF8121, c(3,1,2)))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
