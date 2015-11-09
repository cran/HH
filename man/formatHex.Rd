\name{formatHex}
\alias{formatHex}
\alias{formatBin}
\alias{formatDec}
\alias{scanHex}
\alias{scanBin}
\alias{formatHexInternal}
\alias{HextoBin}
\alias{BintoHex}
\alias{Bintohex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format numbers in binary and hex and decimal format, and scan binary and hex.
}
\description{
Format numbers in binary and hex and decimal format, and scan binary and
hex.
This is an early version of functions which will eventually be in Rmpfr.
}
\usage{
formatHex(x, ...)

formatBin(x, precBits=min(getPrec(x)), scientific=TRUE,
          left.pad="_", right.pad=left.pad, ...)

formatDec(x, precBits = min(getPrec(x)), digits=decdigits,
          nsmall=NULL, scientific=FALSE, ...)

scanBin(x, precBits=stop("Must specify precBits.", call.=FALSE), scientific=TRUE)

scanHex(x, precBits=NULL)

## not exported
formatHexInternal(x, precBits=min(Rmpfr::getPrec(x)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    For the \code{formatXXX} functions, any \code{numeric} or \code{mpfr} object.
    For the \code{scanXXX} functions, the result of a previous
    \code{formatXXX} function.
}
  \item{precBits}{
    Number of bits of precision.
The default value is the number returned by \code{Rmpfr::mpfr_default_prec()}
    Double precision numbers have 53
    bits.  For more detail, see \code{\link[Rmpfr]{mpfr}}.

    For the \code{scanHex} and \code{scanBin} functions, the function
    will detect the precision of the input object unless
    \code{shift=TRUE}.
    When \code{shift=TRUE}, the \code{scanBin} will take the default precision
    (double precision with \code{Rmpfr::mpfr_default_prec()} bits,
    currently 53 bits).
  }
\item{scientific}{
  Logical.  When \code{TRUE} (the default) \code{formatBin}  will display
    the binary representation in scientific notation (\code{mpfr(3, 5)} is displayed as
  \code{+0b1.1000p+1}).  When \code{FALSE},  \code{formatBin}  will display
    the binary representation in
  regular format shifted to align binary points (\code{mpfr(3, 5)} is displayed
  \code{+0b11.000}).  When \code{FALSE}, \code{scanBin} will substitute
    \code{0}  for \code{_}.
  }
\item{\dots}{Additional arguments.
  \code{formatHexInternal}, \code{formatHex}, \code{formatBin}: \code{precBits}
  is the only \code{...} argument acted on.
Other \code{...} arguments are ignored.

\code{formatDec}: \code{precBits} is acted on.  Any argument accepted by
\code{\link[base]{format}} (except \code{digits} and \code{nsmall}) is
acted on.
Other \code{...} arguments are ignored.
}
  \item{left.pad, right.pad}{
Characters that will be used for left- and right-padding of the
formatted string when \code{shift=TRUE}.
}
\item{digits, nsmall}{arguments forwarded to \code{format}.
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
specified precision and then sent to the \code{format} function for
\code{scientific=FALSE} or to the sprintf function for
\code{scientific=TRUE}.

\code{scanBin} reads the output of \code{formatBin}.
\code{scanHex} reads the output of \code{formatHex}.

}
\value{
For the \code{formatXXX} functions,
  a character matrix containing the formatted represention of the
argument.
Arithmetic cannot be done on the result.

For the \code{scanXXX} functions, a \code{mpfr} object containing the
value of the argument.
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

formatHex(FourBits)
formatBin(FourBits)
formatBin(FourBits, shift=TRUE)
formatDec(FourBits)

FBB <- formatBin(FourBits)
scanBin(FBB, precBits=4)

FBH <- formatHex(FourBits)
scanHex(FBH)

TenFrac <- matrix((1:10)/10, dimnames=list(1:10, expression(1/x)))
TenFrac9 <- Rmpfr::mpfr(TenFrac, precBits=9) ## 9 significant bits
TenFrac9
formatHex(TenFrac9)
formatBin(TenFrac9)
formatBin(TenFrac9, shift=TRUE)
formatDec(TenFrac9)
formatDec(TenFrac9, precBits=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ classes }
\keyword{ arith }% __ONLY ONE__ keyword per line
