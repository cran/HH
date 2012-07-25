\name{gof.calculation}
\alias{gof.calculation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Box--Ljung Goodness of Fit for ARIMA models in S-Plus.}
\description{
  Calculate Box--Ljung Goodness of Fit for ARIMA models in S-Plus.
  In R we use the \code{Box.test} function.
}
\usage{
gof.calculation(acf.list, gof.lag, n, n.parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf.list}{An \code{"acf"} object.}
  \item{gof.lag}{The number of model parameters is the number of lags to
    use for computing the Portmanteau goodness of fit statistic}
  \item{n}{Number of residuals in model.}
  \item{n.parms}{Number of AR and MA parameters in the model.}
}
\details{
  This function is isolated from the S-Plus \code{arima.diag} function.
  It is used only in S-Plus.
}
\value{
  See the \code{gof} value described in
%% R
#ifndef S-Plus
  \code{arima.diag}
#endif
#ifdef S-Plus  
  \code{\link[splus]{arima.diag}}
#endif
  in S-Plus.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\seealso{
%% R
#ifndef S-Plus
  \code{arima.diag}
#endif
#ifdef S-Plus  
  \code{\link[splus]{arima.diag}}
#endif
  in S-Plus. }
\examples{
if.R(s={
co2.arima <- arima.mle(co2, list(list(order=c(0,1,1)),
                                 list(order=c(0,1,1), period=12)))
co2.acf <- acf(resid(co2.arima), plot=FALSE, lag=40)
co2.gof <- gof.calculation(co2.acf, 36, length(co2), 2)
xyplot(p.value ~ lag, data=co2.gof, panel=panel.gof,
       ylim=range(0, co2.gof$p.value))
},r={})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
