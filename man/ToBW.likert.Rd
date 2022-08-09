\name{ToBW.likert}
\alias{ToBW.likert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change colors in a likert plot to shades of Black and White.
}
\description{
  Change colors in a likert plot to shades of Black and White.  This
  function is tailored for a \code{\link{likert}} plot, an example of a
  \code{"trellis"} object.  \code{likert} is based on
  \code{\link{panel.bwplot}}.  There are other places in the structure of
  a more general \code{"trellis"} object where colors are stored.
  The specifics for this plot is (1) that the colors for negative values in
  the plot are in reverse order and (2) the color for a neutral-position panel appears on
  both the positive and negative side.  The default values are for
  three items on the negative side, two on the positive side, and no
  neutral.
  See the examples for an example with a neutral.
}
\usage{
   ToBW.likert(x,
               colLegendOrder=c("gray85", "gray20", "gray60", "gray75", "gray45"),
##                               ^Ask      Refu      ^Imp    | Impt      Essn
##	       
               colBarchartOrder=colLegendOrder[c(3,2,1,4,5)],
##                               ^Imp      Refu      ^Ask    | Impt      Essn
               columns=5)
## negative colors are in reverse order in the BarchartOrder
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{"trellis"} object, specifically one constructed by the \code{\link{likert}} function.
}
  \item{colLegendOrder}{
Revised value of \code{x$legend$bottom$args$rect$col}
}
  \item{colBarchartOrder}{
Revised value of both \code{x$panel.args.common$col}
  \code{x$panel.args.common$border}.
}
  \item{columns}{
Revised value of \code{x$legend$bottom$args$columns}
}
}
\value{
\code{"trellis"} object, identical to the input object except for the colors.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link{likert}}
}
\examples{
tmp <- array(1:20, c(4, 5),
             list(letters[1:4],
                  c("NotAsked","VeryNegative","Negative","Positive","VeryPositive")))
tmp

Ltmp <- likert(tmp, ReferenceZero=3.5, col=c("gray85", likertColor(4)), as.percent=TRUE)
Ltmp

ToBW.likert(Ltmp)

## with neutral

tmp2 <- array(1:20, c(4, 5),
             list(letters[1:4],
                  c("VeryNegative","Negative","Neutral","Positive","VeryPositive")))
tmp2

Ltmp2 <- likert(tmp2, ReferenceZero=3, col=likertColor(5),
                as.percent=TRUE, main="Neutral")
Ltmp2

ToBW.likert(Ltmp2, 
            colLegendOrder=c("gray20", "gray60", "gray85", "gray75", "gray45"),
##                                               Neu|tral 
            colBarchartOrder=c("gray85", "gray60", "gray20",  "gray85", "gray75", "gray45")
##                         Neutral left                        Neutral right
)

update(main="Wrong way to handle neutral",
       ToBW.likert(Ltmp2, 
                   colLegendOrder=c("gray20", "gray60", "gray85", "gray75", "gray45"))
      )

}
