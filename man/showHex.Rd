\name{showHex}
\alias{showHex}
\alias{showBin}
\alias{showHexInternal}
\alias{HextoBin}
\alias{BintoHex}
\alias{Bintohex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show numbers in binary and hex format
}
\description{
Show numbers in binary and hex format.
}
\usage{
showHex(x, ...)
## hexdigits is the number of hex digits after the precision point

showBin(x, ..., shift=FALSE, LeftPad="_", RightPad="_")
## bindigits is number of binary digits after the precision point

## not exported
showHexInternal(x,
                bindigits=if (is(x, "mpfr"))
                   min(Rmpfr::.getPrec(x))
                else
                   Rmpfr::mpfr_default_prec(),
                hexdigits=1 + ((bindigits-1) \%/\% 4),
                ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x
}
  \item{bindigits}{
    Number of binary digits after the binary point.
  }
  \item{hexdigits}{
    Number of hexadecimal digits after the hexadeximal point.
}
\item{shift}{
  Logical.  When \code{FALSE} (the default) the binary representation is
  displayed in scientific notation (3 is displayed as
  \code{+0b1.1000p+1}).  When \code{TRUE}, the binary representation is in
  regulat format shifted to align binary points (3 is displayed
  \code{+0b11.000}.
}
  \item{\dots}{
The specifications bindigits and hexdigits are the only optional
arguments that will be used.
}
  \item{LeftPad, RightPad}{
Characters that will be used for left- and right-padding of the
formatted string when \code{shift=TRUE}.
}
}
\details{
For the hexadecimal representation, the function
\code{\link[base]{sprintf}} is used directly.  For the binary
representation, the hexadecimal value is calculated and then edited by
substitution of the binary representation of the hex characters coded in
the \code{HextoBin} vector.
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
\author{
rmh
}
\seealso{
\code{\link[Rmpfr]{mpfr}}, \code{\link[base]{sprintf}}
}
\examples{
FourBits <- Rmpfr::mpfr(matrix(0:31, 8, 4), precBits=4) ## 4 significant bits
dimnames(FourBits) <- list(0:7, c(0,8,16,24))
FourBits

showHex(FourBits)
showBin(FourBits)
showBin(FourBits, shift=TRUE)

showBin(FourBits, bindigits=3) ## 3 digits after the binary point
showBin(FourBits, bindigits=3, shift=TRUE)

showHex(FourBits, bindigits=6)
showHex(FourBits, hexdigits=2)

showBin(FourBits, bindigits=6)
showBin(FourBits, shift=TRUE, bindigits=6)
showBin(FourBits, hexdigits=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ classes }
\keyword{ arith }% __ONLY ONE__ keyword per line
