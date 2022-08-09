\name{EmphasizeVerticalPanels}
\alias{EmphasizeVerticalPanels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper function for likertWeighted(). used for vertical spacing and
horizontal borders of grouped panels.
}
\description{
Helper function for \code{likertWeighted()} used for vertical spacing and
horizontal borders of grouped panels.  Horizontal rules between panels
are suppressed by default by \code{likertWeighted} unless \code{y.between} is
non-zero.
See examples.
}
\usage{
EmphasizeVerticalPanels(x, y.between)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{"trellis"} object, normally one constructed by \code{\link{likertWeighted}}.
}
\item{y.between}{
  The \code{between=list(y=numericvector)} argument applied to a trellis object.
}
}
\value{
Revised \code{trellis} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{likertWeighted}}
}
\examples{
tmp1 <- array(1:60, c(5, 4, 3), list(letters[1:5],letters[6:9],letters[10:12]))
tmp2 <- toCQxR(tmp1)
colnames(tmp2)

likertWeighted(~ . | group + row, tmp2)

likertWeighted(~ . | group + row, tmp2, h.resize.panels=1:5,
               between=list(y=c(0,0,3,0)),
               h.resizePanels=1:5,
               ylab=c("Bottom","Top"),
               xlab.top=c("First","Second","Third"),
               auto.key.title="Response Level",
               main="Three Questions by Five Levels of Classification")

likertObject <- likertWeighted(~ . | group + row, tmp2)
likertObject

EmphasizeVerticalPanels(likertObject, y.between=c(0,0,1,0))

}
