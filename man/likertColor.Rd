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
likertColor(nc, ReferenceZero=NULL,
                colorFunction=c("diverge_hcl","sequential_hcl"),
                colorFunctionOption=c("lighter","flatter","default"),
                colorFunctionArgs=
                  likertColorFunctionArgs[[colorFunctionOption, colorFunction]],
                ...)
likertColorBrewer(nc, ReferenceZero=NULL,
                  BrewerPaletteName="RdBu", middle.color="gray90")

brewer.pal.likert(n, name, middle.color)
}

\arguments{
  \item{n, nc}{Number of colors in the palette.
    If there are more levels than \code{RColorBrewer} normally handles, we
    automatically interpolate with
    \code{\link[grDevices]{colorRampPalette}}.
  }
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
    label \code{"Neutral"}.
  }
  \item{colorFunction}{Function name from the \pkg{colorspace} package,
    either \code{"diverge_hcl"} or\cr
    \code{"sequential_hcl"}.}
  \item{colorFunctionOption}{Name of a list item defined inside the
    \code{likertColor}
  function.  The item contains a list of parameters to the function
  identified in the \code{colorFunction} argument.}
  \item{colorFunctionArgs}{list of arguments to the \pkg{colorspace}
    function.
  The default selects the values by indexing into a list defined in the
  \code{likertColor} function using the values of the two arguments
  \code{colorFunction} and  \code{colorFunctionOption}}.  For
  non-default usage, see the BlueOrange example in this help page.
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
  For balanced levels, such as\cr
  \code{c("Disagree Strongly", "Disagree Weakly", "Agree
    Weakly", "Agree Strongly")},\cr
    corresponding to \code{nc=4,
  ReferenceZero=2.5}, it returns \code{-2 -1  1  2}.
  For unbalanced levels, such as \code{c("Disagree", "Neutral", "Agree
  Weakly", "Agree Strongly")}, corresponding to \code{nc=4,
  ReferenceZero=2}, it returns \code{-1 0  1  2}.

  \code{likertColor} returns a subset of a palette constructed by
  either \code{\link[colorspace]{diverge_hcl}} or
  \code{\link[colorspace]{sequential_hcl}} in the
  \pkg{colorspace} package.
  The subset corresponds to the levels specified by \code{ColorSet}.

  \code{brewer.pal.likert} returns a
  \code{\link[RColorBrewer]{RColorBrewer}} palette.

  \code{likertColorBrewer} returns a subset of a palette constructed by
  \code{brewer.pal.likert}.
  The subset corresponds to the levels specified by \code{ColorSet}.
}
\details{
These are support functions for the \code{plot.likert} function.
Please see \code{\link{plot.likert}} for details.

\code{likertColor} uses by default the \code{\link[colorspace]{diverge_hcl}}
diverging palette defined by the argument\cr
\code{colorFunctionOption="lighter"}.

\code{likertColorBrewer} by default uses the \code{"RdBu"}
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

\dontrun{
  ## Examples illustrating the six predefined likertColor palettes, and how
  ## to define additional hcl color palettes for use with the likert functions.

  data(ProfDiv)
  ProfDiv.df <- data.frame(ProfDiv)

  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE)
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
         colorFunctionOption="default")
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
         colorFunctionOption="flatter")
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl")
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl", colorFunctionOption="default")
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl", colorFunctionOption="flatter")

  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE)
  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
         colorFunctionOption="default")
  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
         colorFunctionOption="flatter")
  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl")
  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl", colorFunctionOption="default")
  likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
         colorFunction="sequential_hcl", colorFunctionOption="flatter")

  likertMosaic(ProfDiv.df)
  likertMosaic(ProfDiv.df, colorFunctionOption="default")
  likertMosaic(ProfDiv.df, colorFunctionOption="flatter")
  likertMosaic(ProfDiv.df, colorFunction="sequential_hcl")
  likertMosaic(ProfDiv.df, colorFunction="sequential_hcl",
               colorFunctionOption="default")
  likertMosaic(ProfDiv.df, colorFunction="sequential_hcl",
               colorFunctionOption="flatter")

  ## specify an hcl palette for use with the likert functions.
  BlueOrange <- likertColor(nc=4, ReferenceZero=NULL,
                            colorFunction="diverge_hcl",
                            colorFunctionArgs=
                              list(h=c(246, 40), c=96, l=c(65,90), power=1.5))
  likert( ~ . , ProfDiv.df, horizontal=FALSE, positive.order=FALSE, col=BlueOrange)
}
}
\author{Richard M. Heiberger,
        with contributions from Naomi B. Robbins <naomi@nbr-graphs.com>.

  Maintainer: Richard M. Heiberger <rmh@temple.edu>
}
\keyword{hplot}
