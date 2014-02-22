\name{as.likert}
\alias{is.likert}
\alias{as.likert}
\alias{as.likert.data.frame}
\alias{as.likert.formula}
\alias{as.likert.ftable}
\alias{as.likert.table}
\alias{as.likert.matrix}
\alias{as.likert.listOfNamedMatrices}
\alias{as.likert.array}
\alias{as.likert.default}
\alias{is.likertCapable}
\alias{rev.likert}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Support functions for diverging stacked barcharts for Likert, semantic
differential, and rating scale data.}
\description{
Constructs class="likert" objects to be used by the plot.likert methods.
}
\usage{

is.likert(x)

as.likert(x, ...)
\method{as.likert}{default}(x, ...)
\method{as.likert}{data.frame}(x, ...)
\method{as.likert}{formula}(x, ...)  ## doesn't work yet
\method{as.likert}{ftable}(x, ...)
\method{as.likert}{table}(x, ...)
\method{as.likert}{matrix}(x,
                ReferenceZero=NULL,
                ...,
                rowlabel=NULL, collabel=NULL,
                xlimEqualLeftRight=FALSE,
                xTickLabelsPositive=TRUE,
                padding=FALSE,
                reverse.left=TRUE)
\method{as.likert}{listOfNamedMatrices}(x, ...)
\method{as.likert}{array}(x, ...)
%% \method{as.likert}{numeric}(x, ...)

\method{rev}{likert}(x)

is.likertCapable(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{For the \code{as.likert} methods, a numeric object stored as a vector, matrix,
    two-dimensional table, two-dimensional ftable,
    two-dimensional structable (as defined in the \code{vcd} package),
    or list of named matrices.
    For \code{is.likert} and \code{is.likertCapable}, any object.
    This is the only required argument.}
  \item{rowlabel, collabel}{\code{names(dimnames(x))}, where \code{x} is
    the argument to the \code{as.likert} functions.
    These will become the \code{xlab} and \code{ylab} of the likert plot.}
  \item{\dots}{other arguments.  They will be ignored by
    the \code{as.likert} method.}
  \item{ReferenceZero}{Please see discussion of this argument
    in \code{\link{likert}}.}
  \item{xlimEqualLeftRight}{Logical. The default is \code{FALSE}.  If
    \code{TRUE}, then the left and right x limits are set to negative
    and positive of the larger of the absolute value of the original x limits.}
  \item{xTickLabelsPositive}{Logical. The default is \code{TRUE}.
    If \code{TRUE}, then the tick labels on the negative side are
    displayed as positive values.}

  \item{padding, reverse.left}{\code{padding} is \code{FALSE} for
    \code{likert} and \code{TRUE} for \code{likertMosaic}.
    \code{reverse.left} is \code{TRUE} for \code{likert} and \code{FALSE}
    for \code{likertMosaic}.  \code{likert} is based on
    \code{\link[lattice]{barchart}}
    and requires that the sequencing of negative values be reversed. \code{likertMosiac} is
    based on \code{\link[vcd]{mosaic}} and needs padding on left and right
    to fill the rectangle implied by the convex hull of the plot.
  }

  }
\details{
Please see \code{\link{likert}} for information on the plot for which
\code{as.likert} prepares the data.
}

\value{
 For the \code{as.likert} methods, a \code{likert} object, which is
 a matrix with additional attributes that are needed to make the
 \code{barchart} method used by the \code{\link{plot.likert}} methods
 work with the data.  Columns for respondents who
 disagree have negated values.
 The column of the original data for respondents who
neither agree nor disagree is split into two columns, each containing
halved values---one positive and one negative.
Negative columns come first in the sequence of "No Opinion"(negative)--"Strongly
Disagree",
followed by "No Opinion"(positive)--"Strongly Agree".
There are four
 attributes:
 \code{"even.col"} indicating whether there were originally an even
 number of columns, \code{"n.levels"} the original number of levels,
 \code{"levels"}
 the original levels in the original order, \code{"positive.order"}
 The sequence in which to display the rows in order to make the right
 hand sides progress with high values on top.

 \code{is.likert} returns a \code{TRUE} or \code{FALSE} value.

 \code{is.likertCapable} returns a \code{TRUE} or \code{FALSE} value
 if the argument can used as an argument to one of the \code{plot.likert} methods.

}
\seealso{\code{\link{likert}}}
\references{
  Richard M. Heiberger, Naomi B. Robbins (2014).,
        "Design of Diverging Stacked Bar Charts for Likert Scales and Other Applications",
        Journal of Statistical Software, 57(5), 1--32,
        \url{http://www.jstatsoft.org/v57/i05/.}

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
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
## Please see ?likert to see these functions used in context.

tmp2 <- array(1:12, dim=c(3,4), dimnames=list(B=LETTERS[3:5], C=letters[6:9]))
as.likert(tmp2)  ## even number of levels.

is.likert(tmp2)
is.likert(as.likert(tmp2))
}
\keyword{hplot}
