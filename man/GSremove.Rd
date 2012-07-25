\name{GSremove}
\alias{GSremove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove selected GraphSheetPages in the S-Plus Windows GUI Graphsheet}
\description{
  Remove selected GraphSheetPages in the S-Plus Windows GUI Graphsheet.
  This does the same task as right-click/delete on the tabs of the GraphSheet.
}
\usage{
GSremove(pages, sheet = "GSD2$Page")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pages}{Page numbers in the tabs at the bottom of the Graphsheet.}
  \item{sheet}{Defaults to \code{GSD2}, the first name that is used when
  the \code{graphsheet} or \code{trellis.device} function is used.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
  \code{graphsheet}
#endif
#ifdef S-Plus  
  \code{\link[splus]{graphsheet}}
#endif
  in S-Plus.}
\examples{
\dontrun{
trellis.device()
plot(1:10); plot(11:20); plot(21:30)
GSremove(c(1,3))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
