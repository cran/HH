\name{likertColor}
\alias{likertColor}
\alias{likertColorBrewer}
\alias{brewer.pal.likert}
\alias{ColorSet}

\title{Selection of colors for Likert plots.}
\description{
Selection of colors for Likert plots.
}
\usage{
ColorSet(nc, ReferenceZero=NULL)
likertColor(nc, ReferenceZero=NULL, ...)
likertColorBrewer(nc, ReferenceZero=NULL, BrewerPaletteName="RdBu", middle.color="gray90")

brewer.pal.likert(n, name, middle.color)
}

\arguments{
  \item{n, nc}{Number of colors in the palette.
    If there are more levels than \code{RColorBrewer} normally handles, we
    automatically interpolate with
    \code{\link[grDevices]{colorRampPalette}}.
  }
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
    label \code{"Neutral"}.
  }
  \item{\dots}{Other arguments are ignored.}
  \item{BrewerPaletteName, name}{\code{\link[RColorBrewer]{RColorBrewer}}
    palette names. We default to the diverging palette \code{RdBu}.
    Diverging palettes are usually appropriate for two-directional
    scales (Agree--Disagree).  Sequential palettes are often appropriate
    for one-directional scales (Age Ranges).  Qualitative palettes are
    usually not appropriate for likert plots.
  }
  \item{middle.color}{Darker middle color than the default
    \code{"#F7F7F7"} in the \code{RdBu} scheme.}
}
\value{
  \code{ColorSet} returns a vector of integers, one per each level,
  corresponding to the strength of the levels from Disagree to Agree.
  For balanced levels, such as
  \code{c("Disagree Strongly", "Disagree Weakly", "Agree
  Weakly", "Agree Strongly")}, corresponding to \code{nc=4,
  ReferenceZero=2.5}, it returns \code{-2 -1  1  2}.
  For unbalanced levels, such as \code{c("Disagree", "Neutral", "Agree
  Weakly", "Agree Strongly")}, corresponding to \code{nc=4,
  ReferenceZero=2}, it returns \code{-1 0  1  2}.

  \code{brewer.pal.likert} returns a
  \code{\link[RColorBrewer]{RColorBrewer}} palette.

  \code{likertColor} returns a subset of a palette constructed by
  \code{brewer.pal.likert}.
  The subset corresponds to the levels specified by \code{ColorSet}.
}
\details{
These are support functions for the \code{plot.likert} function.
Please see \code{\link{plot.likert}} for details.
\code{likertColor} uses the \code{\link[colorspace]{diverge_hcl}} default
  diverging palette.  \code{likertColorBrewer} by default uses the \code{"RdBu"}
  diverging palette from \code{\link[RColorBrewer]{RColorBrewer}}.
}
\seealso{\code{\link{plot.likert}}}
\examples{
brewer.pal.likert(4, "RdBu")
brewer.pal.likert(5, "RdBu")
ColorSet(4)
ColorSet(4, 2)
likertColor(4)
likertColor(4, 2.5) ## same as above
likertColor(4, 2)   ## one negative level and two positive levels: default
likertColor(5, 3)[-2] ## one negative level and two positive levels: stronger negative
}
\author{Richard M. Heiberger, with contributions from Naomi B. Robbins <naomi@nbr-graphs.com>.

  Maintainer: Richard M. Heiberger <rmh@temple.edu>
}
\keyword{hplot}
