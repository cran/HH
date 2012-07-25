\name{panel.pairs.hh}
\alias{panel.pairs.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function based on S-Plus panel.pairs to add the subpanel.scales
  and panel.cex arguments.}
\description{
  Function based on S-Plus \code{panel.pairs} to add the \code{subpanel.scales} and
   \code{panel.cex} arguments.  In R, this is an alias for \code{panel.pairs}.
}
\usage{
panel.pairs.hh(x, y, z, subscripts, pscales, subpanel = panel.splom,
           varnames = dimnames(x)[[2]], ...,
           subpanel.scales, panel.cex=par()$cex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x, y, z, subscripts, pscales, subpanel, varnames, \dots}{See
%% R
#ifndef S-Plus
    \code{splom}
#endif
#ifdef S-Plus
    \code{\link[trellis]{splom}}
#endif
    in S-Plus.}
  \item{subpanel.scales}{Controls the size of the tick labels in the
  diagonal panel.}
  \item{panel.cex}{Controls the size of the variable names in the
  diagonal panel.}
}
\value{
\code{"trellis" object}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
    \code{splom}
#endif
#ifdef S-Plus
  \code{\link[trellis]{splom}}
#endif
  in S-Plus. }
\examples{
if.R(s={
     longley <- data.frame(longley.x, Employed = longley.y)
     },r={
     data(longley)
     })

if.R(s=
     splom( ~ longley, pch=16, cex=.55,
           superpanel=panel.pairs.hh, subpanel.scales=list(cex=.8),
           pscales=2,
           panel.cex=.8)
     ,r=
     splom( ~ longley, pch=16,
           pscales=2,
           varname.cex=.8,
           axis.text.cex=.5)
   )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
