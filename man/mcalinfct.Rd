\name{mcalinfct}
\alias{mcalinfct}
%% \alias{meanslinfct.hh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MCA multiple comparisons analysis (pairwise)}
\description{
MCA multiple comparisons analysis (pairwise).  We calculate the
contrast matrix for all pairwise comparisons, taking account of
covariates and interactions.
}
\usage{
mcalinfct(model, focus,
          mmm.data=model$model,
          formula.in=terms(model),
          linfct.Means=
%%           meanslinfct.hh(model, focus, mmm.data, formula.in,
          %% multcomp:::meanslinfct(model, focus, mmm.data, formula.in,
          multcomp.meanslinfct(model, focus, mmm.data, formula.in,
                         contrasts.arg=model$contrasts),
          type="Tukey"
          )
%% meanslinfct.hh(model, focus, mmm.data = model$model,
%%                formula.in = terms(model), contrasts.arg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{\code{aov} object}
  \item{focus}{name of one of the factors in the model, as a character object.}
  \item{mmm.data}{\code{data.frame} from which the model was estimated.
  Normally, the default is the correct value.}
  \item{formula.in}{\code{formula} of the model which was estimated.
  Normally, the default is the correct value.  The use of the
  \code{terms}
  function honors the \code{keep.order=TRUE} if it was specified.}
  \item{linfct.Means}{Contrast matrix for the adjusted means of each
    level of the focus factor.   Normally, the default is the correct value.}
  \item{type}{Name of the multiple comparison procedure to be used.
    See \code{\link[multcomp]{contrMat}}.}
%%  \item{contrasts.arg}{argument to \code{model.matrix}.}
  }
\value{
  Matrix to be used as a value for the \code{linfct} argument to
  \code{\link[multcomp]{glht}}.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\note{This function provides results similar to the
  \code{mcp(focusname="Tukey")} argument to \code{glht}.
  I think it provides better values for covariate and interaction terms.
  }
\seealso{ \code{\link{MMC}}}
\examples{
## See the examples in HH/scripts/MMC.cc176.R
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
