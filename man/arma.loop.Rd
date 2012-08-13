\name{arma.loop}
\alias{arma.loop}
\alias{diag.arma.loop}
\alias{rearrange.diag.arma.loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Loop through a series of ARIMA models and display coordinated
  tables and diagnoastic graphs.}
\description{
  Loop through a series of ARIMA models and display coordinated
  tables and diagnostic graphs.  The complete example from the Heiberger
  and Teles article, also included in the Heiberger and Holland book, is
  illustrated.
}
\usage{
arma.loop(x,
          model,             ## S-Plus
          order, seasonal,   ## R
          series=deparse(substitute(x)), ...)

diag.arma.loop(z,
               x=stop("The time series x is needed in S-Plus when p=q=0."),
               lag.max = 36, gof.lag = lag.max)

rearrange.diag.arma.loop(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Time series vector.  In S-Plus, \code{x} must be an \code{"rts"}.}
  \item{model}{A valid S-Plus \code{model} for
%% R
#ifndef S-Plus
    \code{arima.mle}.
#endif
#ifdef S-Plus
    \code{\link[splus]{arima.mle}}.
#endif
  }
  \item{order, seasonal}{A valid R \code{order} and \code{seasonal} for
%% R
#ifndef S-Plus
    \code{\link[stats]{arima}}.
#endif
#ifdef S-Plus
    \code{arima}.
#endif
  }
  \item{series}{Character string describing the time series.}
  \item{\dots}{Additional arguments for \code{arima.mle} or \code{arima}.}

  \item{z}{For \code{diag.arma.loop}, an "arma.loop" object.
  For  \code{rearrange.diag.arma.loop}, an "diag.arma.loop" object.}
  \item{lag.max}{Maximum lag for the acf and pacf plots.}
  \item{gof.lag}{Maximum lag for the gof plots.}
}
\details{
  S-Plus and R have different functions, with different input argument names
  and different components in their value.
}
\value{

  \code{arma.loop}: \code{"arma.loop"} object which is a matrix of
  lists, each containing an arima model.

  \code{diag.arma.loop}: \code{"diag.arma.loop"} object which is a
   matrix of lists, each containing the standard diagnostics for one
   arima model.

   \code{rearrange.diag.arma.loop}: List of matrices, each containing
   all the values for a specific diagnostic measure collected from the
   set of arima models.
}
\references{
  "Displays for Direct Comparison of ARIMA Models"
  The American Statistician, May 2002, Vol. 56, No. 2, pp. 131-138.
  Richard M. Heiberger, Temple University, and
  Paulo Teles, Faculdade de Economia do Porto.
  
  Richard M. Heiberger and Burt Holland (2004), Statistical Analysis and Data
  Display, Springer, ISBN 0-387-40270-5

}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{\code{\link{tsdiagplot}}}
\examples{
## see tsdiagplot for the example
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ hplot }
