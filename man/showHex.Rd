\name{showHex}
\alias{showHex}
\alias{showBin}
\alias{showDec}
\alias{showHexInternal}
%\alias{HextoBin}
%\alias{BintoHex}
%\alias{Bintohex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show numbers in binary and hex and decimal format
}
\description{
  Show numbers in binary and hex and decimal format.
  showHex is a very early version of formatHex, which will eventually be
  in Rmpfr.
  Use the HH version of formatHex until the Rmpfr version is released.
}
\usage{
## not exported
showHex(x, ...)

## not exported
showBin(x, ..., shift=FALSE, LeftPad="_", RightPad="_")

## not exported
showDec(x, ..., digits, nsmall)

## not exported
showHexInternal(x, precBits=min(Rmpfr::getPrec(x)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Any numeric or mpfr object.
}
  \item{precBits}{
    Number of bits of precision.
The default value is the number returned by \code{mpfr::.getPrec}
    Double precision numbers have 53
    bits.  For more detail, see \code{\link[Rmpfr]{mpfr}}.
  }
\item{shift}{
  Logical.  When \code{FALSE} (the default) the binary representation is
  displayed in scientific notation (mpfr(3, 5) is displayed as
  \code{+0b1.1000p+1}).  When \code{TRUE}, the binary representation is in
  regular format shifted to align binary points (mpfr(3, 5) is displayed
  \code{+0b11.000}).
}
\item{\dots}{Additional arguments.
  \code{showHexInternal}, \code{showHex}, \code{showBin}: \code{precBits}
  is the only \code{...} argument acted on.
Other \code{...} arguments are ignored.

\code{showDec}: \code{precBits} is acted on.  Any argument accepted by
\code{\link[base]{format}} (except \code{digits} and \code{nsmall}) is
acted on.
Other \code{...} arguments are ignored.
}
  \item{LeftPad, RightPad}{
Characters that will be used for left- and right-padding of the
formatted string when \code{shift=TRUE}.
}
\item{digits, nsmall}{These arguments are intercepted by \code{showDec}
  and ignored.
  \code{showDec} generates its own values for these arguments from
  \code{precBits}
  and forwards those values to \code{format}.
  }
}
\details{
For the hexadecimal representation, the function
\code{\link[base]{sprintf}} is used directly.  For the binary
representation, the hexadecimal value is calculated and then edited by
substitution of the binary representation of the hex characters coded in
the \code{HextoBin} vector.  For binary with \code{shift=TRUE}, the
result of the  \code{shift=FALSE} version is edited to align binary
points.  For the decimal representation, the hexadecimal value is
calculated with the
specified precision and then sent to the \code{format} function.
}
\value{
A character matrix containing the formatted represention of the
argument.
Arithmetic cannot be done on the result.
}
\references{
  R FAQ 7.31:  Why doesn't R think these numbers are equal?
  \code{system.file("../../doc/FAQ")}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[Rmpfr]{mpfr}}, \code{\link[base]{sprintf}}
}
\examples{
FourBits <- Rmpfr::mpfr(matrix(0:31, 8, 4), precBits=4) ## 4 significant bits
dimnames(FourBits) <- list(0:7, c(0,8,16,24))
FourBits

HH:::showHex(FourBits)
HH:::showBin(FourBits)
HH:::showBin(FourBits, shift=TRUE)
HH:::showDec(FourBits)


TenFrac <- matrix((1:10)/10, dimnames=list(1:10, expression(1/x)))
TenFrac9 <- Rmpfr::mpfr(TenFrac, precBits=9) ## 9 significant bits
TenFrac9
HH:::showHex(TenFrac9)
HH:::showBin(TenFrac9)
HH:::showBin(TenFrac9, shift=TRUE)
HH:::showDec(TenFrac9)
HH:::showDec(TenFrac9, precBits=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ classes }
\keyword{ arith }% __ONLY ONE__ keyword per line
