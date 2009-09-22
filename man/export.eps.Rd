\name{export.eps}
\alias{export.eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exports a graph to an EPS file.}
\description{
  Exports a graph from the current device in R,
  or the \code{graphsheet} in S-Plus, to an \code{EPS} file.
}
\usage{
export.eps(FileName.in, Name.in="GSD2", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName.in}{ name of file to be created.}
  \item{Name.in}{Name of graphsheet in S-Plus, ignored in R.}
  \item{\dots}{other arguments in R, ignored in S-Plus.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
%\seealso{\code{\link[grDevices]{dev2}} in R,
%  \code{\link[sgui]{export.graph}} in S-Plus.}

\seealso{
%% R
#ifndef S-Plus
  \code{\link[grDevices]{dev2}}.
#endif
#ifdef S-Plus  
  \code{\link[sgui]{export.graph}}.
#endif
}

% \seealso{\code{\link[grDevices]{dev2}} in R,
%   %\code{\link[sgui]{export.graph}}
%    \code{export.graph}
%   in S-Plus.}

% \seealso{
%   \Sexpr[results=rd]{if.R(s="dev2",
%                           r="\\\\code{\\\\link[grDevices]{dev2}}")}
% 			in R,
%   \Sexpr[results=rd]{if.R(r="export.graph",
%                           s="\\\\code{\\\\link[ssgui]{export.graph}}")}
% 			in S-Plus.
%   }

% \seealso{
%   \Sexpr[results=rd]{if (is.R()) "\\\\code{\\\\link[grDevices]{dev2}}" else "dev2"}
% 			in R,
%   \Sexpr[results=rd]{if (is.R()) "export.graph" else "\\\\code{\\\\link[sgui]{export.graph}}"}
% 			in S-Plus.
% }
  
  \examples{
\dontrun{
trellis.device()
plot(1:10)
export.eps("abcd.eps")
}
}
\keyword{ device }
