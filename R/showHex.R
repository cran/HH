## sprintf("%+13.13a", x) ## hex digits after the hex point = 13

## precBits: double precision = 53 = 1 + 13*4


## conversion from Hex digits to binary sequences of digits
HextoBin <- c(
 "0"="0000",
 "1"="0001",
 "2"="0010",
 "3"="0011",
 "4"="0100",
 "5"="0101",
 "6"="0110",
 "7"="0111",
 "8"="1000",
 "9"="1001",
 "A"="1010",
 "B"="1011",
 "C"="1100",
 "D"="1101",
 "E"="1110",
 "F"="1111",
 "a"="1010",
 "b"="1011",
 "c"="1100",
 "d"="1101",
 "e"="1110",
 "f"="1111")

## the code isn't using either of these inverses.
BintoHex <- names(HextoBin[1:16])
names(BintoHex) <- HextoBin[1:16]

Bintohex <- names(HextoBin[c(1:10,17:22)])
names(Bintohex) <- HextoBin[c(1:10,17:22)]



showHexInternal <-
  function(x, precBits=min(Rmpfr::getPrec(x)), ...) {
    bindigits <- precBits-1
    hexdigits <- 1 + ((bindigits-1) %/% 4)
    ## hexdigits is the number of hex digits after the precision point
    if (missing(bindigits) && !missing(hexdigits)) bindigits <- 4*hexdigits
    format <- paste("%+", as.character(hexdigits), ".", as.character(hexdigits), "a", sep="")
    result <- sprintf(format, x)
    attr(result, "bindigits") <- bindigits
    attr(result, "hexdigits") <- hexdigits
    class(result) <- "noquote"
    result
  }


showHex <- function(x, ...) {
  ## hexdigits is the number of hex digits after the precision point
  result <- showHexInternal(x, ...)
  dim(result) <- dim(x)
  dimnames(result) <- dimnames(x)
  attr(result, "bindigits") <- NULL
  attr(result, "hexdigits") <- NULL
  result
}

showBin <- function(x, ..., shift=FALSE, LeftPad="_", RightPad="_") {
  ## bindigits is number of binary digits after the precision point
  H <- showHexInternal(x, ...)
  bindigits <- attr(H, "bindigits")
  hexdigits <- attr(H, "hexdigits")
  S <- substring(H, 1, 1)
  A <- substring(H, 4, 4)
  B <- substring(H, 6, 6+(hexdigits-1))
  C <- substring(H, 6+hexdigits+1)
  sB <- strsplit(B, "")
  rsB <- do.call(rbind, sB)
  hrsB <- HextoBin[rsB]
  dim(hrsB) <- dim(rsB)
  hrsBa <- apply(hrsB, 1, paste, collapse="")
  hrsBb <- substring(hrsBa, 1, bindigits)
  ## While this is a truncation,
  ## the mpfr conversion assures that
  ## only zero characters are truncated.
  if (shift) {

    LeftPads <- paste(rep(LeftPad, 60), collapse="")
    LeftPads <- substring(LeftPads, 0, 0:60)

    RightPads <- paste(rep(RightPad, 60), collapse="")
    RightPads <- substring(RightPads, 0, 0:60)

    powers <- as.numeric(C)
    Left <- -powers + max(powers)
    Right <- powers - min(powers)
    if (max(abs(powers)) > length(LeftPads))
      warning("Shifted binary out of bounds.", call.=FALSE)
    D <- cbind(S, "0b", LeftPads[Left+1], A, hrsBb, RightPads[Right+1])
    D2 <- apply(D, 1, function(x) do.call(paste, list(x, collapse="")))
    resultB <- paste(substring(D2, 1, max(Left)+min(powers)+4), ".",
                     substring(D2, max(Left)+min(powers)+4+1), sep="")
  }
  else {
    resultA <- cbind(S, "0b", A, ".", hrsBb, "p", C)
    resultB <- apply(resultA, 1, function(x) do.call(paste, list(x, collapse="")))
  }
  dim(resultB) <- dim(x)
  dimnames(resultB) <- dimnames(x)
  class(resultB) <- class(H)
  resultB
}

## showDec1 <- function(x, ...) {
##   H <- showHexInternal(x, ...)
##   precBits <- attr(H, "bindigits") + 1
##   decdigits <- ceiling(log(2^precBits, 10)+1)
##   result <- format(x, digits=decdigits, ...)
##   class(result) <- class(H)
##   result
## }

showDec <- function(x, ..., digits, nsmall) {
  H <- showHexInternal(x, ...)
  precBits <- attr(H, "bindigits") + 1
  decdigits <- ceiling(log(2^precBits, 10))
  Hx <- scan(text=H, quiet=TRUE)
  absHx <- abs(Hx)
  minabsHx <- min(absHx[absHx != 0]) ## actual 0 is wanted here
  logminabsHx <- log(minabsHx %% 1, 10)
  nsmall <- if (is.infinite(logminabsHx))
              (decdigits-1) - log(min(absHx[absHx != 0]), 10)
            else
              ceiling(-logminabsHx)
  result <- format(Hx, digits=decdigits, nsmall=nsmall, ...)
  dim(result) <- dim(x)
  dimnames(result) <- dimnames(x)
  class(result) <- class(H)
  result
}


## formatHex <- function(...) {
##   if (exists("formatHex", envir=environment(Rmpfr::mpfr)))
##     Rmpfr::formatHex(...)
##   else
##     showHex(...)
## }

## formatBin <- function(...) {
##   if (exists("formatBin", envir=environment(Rmpfr::mpfr)))
##     Rmpfr::formatBin(...)
##   else
##     showBin(...)
## }


## formatDec <- function(...) {
##   if (exists("formatDec", envir=environment(Rmpfr::mpfr)))
##     Rmpfr::formatDec(...)
##   else
##     showDec(...)
## }




if (FALSE) {

library(Rmpfr)

FourBits <- mpfr(matrix(0:31, 8, 4), precBits=4)
dimnames(FourBits) <- list(0:7, c(0,8,16,24))
FourBits

FiveBits <- mpfr(matrix(0:31, 8, 4), precBits=5)
dimnames(FiveBits) <- list(0:7, c(0,8,16,24))
FiveBits

HH:::showHexInternal(FourBits)
formatHex(FourBits)
formatHex(FourBits, precBits=5)
formatHex(FourBits, precBits=6)
formatBin(FourBits)
formatBin(FourBits, precBits=5)
formatBin(FourBits, shift=TRUE)
formatBin(FourBits, shift=TRUE, precBits=5)
formatDec(FourBits)
formatDec(FourBits, precBits=5)
formatDec(FourBits, precBits=7)
## formatDec1(FourBits, precBits=7)

HH:::showHexInternal(FiveBits)
formatHex(FiveBits)
formatHex(FiveBits, precBits=5)
formatHex(FiveBits, precBits=6)
formatBin(FiveBits)
formatBin(FiveBits, precBits=5)
formatBin(FiveBits, precBits=6)
formatBin(FiveBits, shift=TRUE)
formatBin(FiveBits, shift=TRUE, precBits=6)
formatDec(FiveBits)
formatDec(FiveBits, precBits=5)
formatDec(FiveBits, precBits=7)

TenPowers <- matrix(10^(-3:5), dimnames=list(-3:5, expression(10^x)))
TenPowers
HH:::showHexInternal(TenPowers)
formatHex(TenPowers)
formatBin(TenPowers)
formatBin(TenPowers, shift=TRUE)
formatDec(TenPowers)
formatDec(TenPowers, scientific=FALSE)
formatDec(TenPowers, precBits=54)
formatDec(TenPowers, precBits=54, scientific=FALSE)

TenFrac <- matrix((1:10)/10, dimnames=list(1:10, expression(1/x)))
TenFrac
formatHex(TenFrac)
formatBin(TenFrac)
formatBin(TenFrac, shift=TRUE)
formatDec(TenFrac)

TenFrac9 <- mpfr(TenFrac, precBits=9)
TenFrac9
formatHex(TenFrac9)
formatBin(TenFrac9)
formatBin(TenFrac9, shift=TRUE)
formatDec(TenFrac9)

Rmpfr::getPrec(TenFrac)
Rmpfr::getPrec(TenFrac9)



Ten <- matrix(1:10 + 0.0, dimnames=list(1:10, "x")) ## + 0.0 forces double precision
Ten
formatHex(Ten)
formatHex(Ten, precBits=4)
formatBin(Ten)
formatBin(Ten, precBits=4)
formatBin(Ten, shift=TRUE)
formatBin(Ten, shift=TRUE, precBits=4)
formatDec(Ten)
## formatDec1(Ten)

Ten4 <- mpfr(Ten, precBits=4)
Ten4
formatHex(Ten4)
formatBin(Ten4)
formatBin(Ten4, shift=TRUE)
formatDec(Ten4)

}
