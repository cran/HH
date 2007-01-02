\name{bwplot.formula}
\alias{bwplot.formula}
\alias{compute.packet.new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Replacement for bwplot.formula and compute.packet in R to allow them
to work with the new "positioned" class.}
\description{
Replacement for \code{bwplot.formula} and \code{compute.packet} in R
trellis to allow them to work with the new "positioned" class.
}
\usage{
bwplot.formula(x, data = NULL, allow.multiple = is.null(groups) || 
    outer, outer = FALSE, auto.key = FALSE, aspect = "fill", 
    panel = "panel.bwplot", prepanel = NULL, scales = list(), 
    strip = TRUE, groups = NULL, xlab, xlim, ylab, ylim, box.ratio = 1, 
    horizontal = NULL,
    drop.unused.levels = lattice.getOption("drop.unused.levels"), 
    ...,
    default.scales =
      if (horizontal)
        list(y = list(tck = 0, alternating = FALSE, rot = 0))
      else
        list(x = list(tck = 0, alternating = FALSE)),
    subscripts = !is.null(groups), 
    subset = TRUE)

compute.packet.new(cond, levels, cond.levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%% bwplot.formula
\item{x, data, allow.multiple, outer, auto.key, aspect, panel, prepanel,
  scales, strip, groups, xlab, xlim, ylab, ylim, box.ratio, horizontal,
  drop.unused.levels, \dots, default.scales, subscripts,
  subset}{See \code{\link[lattice]{bwplot.formula}}.}

%% compute.packet.new
  \item{cond, levels}{standard values used by \code{lattice:::compute.packet}.}
  \item{cond.levels}{}
}

\details{\code{bwplot} makes the assumption that the numeric equivalent
  of a factor \code{f} are always integers from the sequence
  \code{seq(1:length(levels(f)))}.  When the \code{"positioned"} class
  is introduced, this is no longer the right assumption.  The numeric
  values need to be chosen from the \code{"position"} attribute.
}
\value{
  See \code{\link[lattice]{bwplot.formula}}.
}
\author{original by  Deepayan Sarkar
  \email{Deepayan.Sarkar@R-project.org}.
  Modifications by Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[lattice]{bwplot.formula}}.}
\keyword{dplot}
