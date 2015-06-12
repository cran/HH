

## sprintf("%+13.13a", x)

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
  function(x,
           bindigits=if (is(x, "mpfr"))
                       min(Rmpfr::.getPrec(x))
                     else
                       Rmpfr::mpfr_default_prec(),
           hexdigits=1 + ((bindigits-1) %/% 4),
           ...) {
  ## hexdigits is the number of hex digits after the precision point
  if (missing(bindigits) && !missing(hexdigits)) bindigits <- 4*hexdigits
  format <- paste("%+", as.character(hexdigits), ".", as.character(hexdigits), "a", sep="")
  result <- sprintf(format, x)
  attr(result, "bindigits") <- bindigits
  attr(result, "hexdigits") <- hexdigits
  result
}


showHex <- function(x, ...) {
  ## hexdigits is the number of hex digits after the precision point
  result <- showHexInternal(x, ...)
  dim(result) <- dim(x)
  dimnames(result) <- dimnames(x)
  class(result) <- "noquote"
  attr(result, "bindigits") <-
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
  ## while this is a truncation,
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
  class(resultB) <- "noquote"
  resultB
}


if (FALSE) {

FourBits <- Rmpfr::mpfr(matrix(0:31, 8, 4), precBits=4)
dimnames(FourBits) <- list(0:7, c(0,8,16,24))
FourBits

FiveBits <- Rmpfr::mpfr(matrix(0:31, 8, 4), precBits=5)
dimnames(FiveBits) <- list(0:7, c(0,8,16,24))
FiveBits

showHexInternal(FourBits)
showHex(FourBits)
showBin(FourBits)
showBin(FourBits, shift=TRUE)
showHex(FourBits, bindigits=6)
showHex(FourBits, hexdigits=2)
showBin(FourBits, shift=TRUE, bindigits=6)
showBin(FourBits, bindigits=6)
showBin(FourBits, hexdigits=2)

showHexInternal(FiveBits)
showHex(FiveBits)
showBin(FiveBits)
showBin(FiveBits, shift=TRUE)

showHexInternal(1:10)
showHex(1:10)
showBin(1:10)
showBin(1:10, shift=TRUE)

showHexInternal(1:10, bindigits=4)
showHex(1:10, bindigits=4)
showBin(1:10, bindigits=4)
showBin(1:10, shift=TRUE, bindigits=4)

showHexInternal(1:10, hexdigits=1)
showHex(1:10, hexdigits=1)
showBin(1:10, hexdigits=1)
showBin(1:10, shift=TRUE, hexdigits=1)

}
