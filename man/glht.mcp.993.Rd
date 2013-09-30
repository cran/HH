\name{glhtWithMCP.993}
\alias{glhtWithMCP.993}
\alias{mcp2matrix.993}
%\alias{glht.mcp} % defunct
%%\alias{mcp2matrix2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retain averaging behavior that was previously available in glht.}
\description{
  For some ANOVA models with two or more factors, we need to average over
  interaction terms.  These functions use an older version of glht.mcp
  and mcp2matrix to do that averaging.
}
\usage{
glhtWithMCP.993(model, linfct, ...)
mcp2matrix.993(model, linfct)
%%mcp2matrix2(model, linfct, `interaction_average`=FALSE, `covariate_average`=FALSE)
}
%glht.mcp(model, linfct, ..., `interaction_average`=FALSE, `covariate_average`=FALSE)

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model, linfct, \dots}{See \code{\link[multcomp]{glht}}}
%%  \item{interaction_average, covariate_average}{Logical, default \code{FALSE}.}
}
\details{
\code{mcp2matrix} is taken from from \code{multcomp_0.993-2.tar.gz/R/mcp.R}.

%% \code{mcp2matrix2} is styled after mcp2matrix in \code{multcomp_1.0-7}, with
%%              optional averaging of interactions based on
%%              \code{multcomp_0.993-1}.  In addition it offers optional averaging of
%%              covariates to get the same answer as S-Plus \code{multicomp}.
%%              With the default value of both new arguments at
%%              \code{FALSE}, \code{mcp2matrix2} behaves exactly like
%%              \code{mcp2matrix}.

\code{glhtWithMCP.993} is based on \code{glht.mcp} in
\code{multcomp_1.0-0/R/glht.R}
with the call to \code{mcp2matrix} replaced by a call to \code{mcp2matrix.993}.

% \code{glht.mcp} is based on \code{glht.mcp} in
% \code{multcomp_1.0-0/R/glht.R} with two changes.  The call to
% \code{mcp2matrix} is replaced by a call to \code{mcp2matrix2}.  The new
% arguments \code{`interaction_average`} and \code{`covariate_average`} are
% passed to \code{mcp2matrix2} and not forwarded to any other downstream
% function.  With the default value of both new arguments at \code{FALSE},
% \code{mcp2matrix2} behaves exactly like \code{multcomp:::glht.mcp}.
}
\value{
See \code{\link[multcomp]{glht}}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[HH]{MMC}}}
\keyword{ htest }
\keyword{design }
