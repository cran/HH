\name{npar.arma}
\alias{npar.arma}
\alias{npar.rarma}
\alias{npar.sarma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Count the number of parameters in an ARIMA model specification.}
\description{
  Count the number of parameters in an ARIMA model specification.
  When \code{arima==FALSE}, just the AR and MA parameters are counted.
  When \code{arima==TRUE}, then the number of difference parameters are
  also included.
}
\usage{
npar.arma(x, arima=FALSE)
npar.sarma(model, arima=FALSE)
npar.rarma(arma, arima=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{"arima"} object in S-Plus or a \code{"Arima"} object
  in R.}
  \item{model}{A \code{model} specification in the S-Plus style.}
  \item{arma}{A \code{arma} specification in the R style}
  \item{arima}{Logical.  \code{TRUE} is number of differencings is to be
  counted.}
}
\value{
A scalar number giving the count.
}
\author{Richard M. Heiberger (rmh@temple.edu)}
\examples{
co2.arima <-
  if.R(s=
       arima.mle(co2, list(list(order=c(0,1,1)),
                           list(order=c(0,1,1), period=12)))
       ,r=
       arima(co2, 
             order=c(0,1,1),
             seasonal=list(order=c(0,1,1), period=12))
       )

npar.arma(co2.arima)

npar.arma(co2.arima, arima=TRUE)

npar.sarma(list(list(order=c(0,1,1)),
                list(order=c(0,1,1), period=12)))

npar.sarma(list(list(order=c(0,1,1)),
                list(order=c(0,1,1), period=12)),
           arima=TRUE)

if.R(s={},
     r=npar.rarma(co2.arima$arma)
)
if.R(s={},
     r=npar.rarma(co2.arima$arma,
           arima=TRUE)
)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
