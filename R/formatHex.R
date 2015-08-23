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

if(FALSE) {
## the code isn't using either of these inverses.
BintoHex <- names(HextoBin[1:16])
names(BintoHex) <- HextoBin[1:16]

Bintohex <- names(HextoBin[c(1:10,17:22)])
names(Bintohex) <- HextoBin[c(1:10,17:22)]
}


formatHexInternal <- function(x, precBits=min(Rmpfr::getPrec(x)), ...) {
    if (precBits > 53) {                                                                          ## rmh warning
      warning("precBit set to 53.  sprintf does not currently support precBits > 53.", call.=FALSE)
      precBits <- 53
    }
    bindigits <- precBits-1
    hexdigits <- 1 + ((bindigits-1) %/% 4)
    ## hexdigits is the number of hex digits after the precision point
##    if (missing(bindigits) && !missing(hexdigits)) bindigits <- 4*hexdigits                      ## rmh removed
    format <- paste("%+", as.character(hexdigits), ".", as.character(hexdigits), "a", sep="")
    result <- sprintf(format, x)
    attr(result, "bindigits") <- bindigits
    attr(result, "hexdigits") <- hexdigits
    class(result) <- "noquote"
    result
  }


formatHex <- function(x, ...) {
  ## hexdigits is the number of hex digits after the precision point
  result <- formatHexInternal(x, ...)
  dim(result) <- dim(x)
  dimnames(result) <- dimnames(x)
  attr(result, "bindigits") <- NULL
  attr(result, "hexdigits") <- NULL
  result
}

formatBin <- function(x, scientific=TRUE, left.pad = "_", right.pad = left.pad, ...) {  ## scientific   rmh
  ## bindigits is number of binary digits after the precision point
  H <- formatHexInternal(x, ...)
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
  if (!scientific) {  ## scientific   rmh

    left.pads <- paste(rep(left.pad, 60), collapse="")
    left.pads <- substring(left.pads, 0, 0:60)

    right.pads <- paste(rep(right.pad, 60), collapse="")
    right.pads <- substring(right.pads, 0, 0:60)

    powers <- as.numeric(C)
    Left <- -powers + max(powers)
    Right <- powers - min(powers)
    if (max(abs(powers)) > length(left.pads))
      warning("Shifted binary out of bounds.", call.=FALSE)
    D <- cbind(S, "0b", left.pads[Left+1], A, hrsBb, right.pads[Right+1])
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

## formatDec1 <- function(x, ...) {
##   H <- formatHexInternal(x, ...)
##   precBits <- attr(H, "bindigits") + 1
##   decdigits <- ceiling(log(2^precBits, 10)+1)
##   result <- format(x, digits=decdigits, ...)
##   class(result) <- class(H)
##   result
## }

## this is the version I sent to Martin
## formatDec2 <- function(x, ..., digits, nsmall) {
##   H <- formatHexInternal(x, ...)
##   precBits <- attr(H, "bindigits") + 1
##   decdigits <- ceiling(log(2^precBits, 10))
##   Hx <- scan(text=H, quiet=TRUE)
##   absHx <- abs(Hx)
##   minabsHx <- min(absHx[absHx != 0]) ## actual 0 is wanted here
##   logminabsHx <- log(minabsHx %% 1, 10)
##   nsmall <- if (is.infinite(logminabsHx))
##               (decdigits-1) - log(min(absHx[absHx != 0]), 10)
##             else
##               ceiling(-logminabsHx)
##   result <- format(Hx, digits=decdigits, nsmall=nsmall, ...)
##   dim(result) <- dim(x)
##   dimnames(result) <- dimnames(x)
##   class(result) <- class(H)
##   result
## }

formatDec <- function(x, displaydigits=decdigits, digits, nsmall, ...) {
  H <- formatHexInternal(x, ...)
  precBits <- attr(H, "bindigits") + 1
  decdigits <- ceiling(log(2^precBits, 10)) + 1
  Hx <- scan(text=H, quiet=TRUE)

  Hx.range <- range(abs(Hx[Hx != 0])) ## excluding hard zero
  digits <- max(decdigits, displaydigits)
  nsmall <- -floor(log(Hx.range[1], 10))
  if (nsmall <= 0) nsmall <- max(nsmall + digits, 0) ## effective only with scientific=FALSE

  result <-
    if (!is.null(list(...)$scientific) && list(...)$scientific) {
      sprintf.format <- paste("%+", 1, ".", as.character(digits), "e", sep="")
      sprintf(sprintf.format, Hx)
    }
    else
      format(Hx, digits=digits, nsmall=nsmall, ...)
  dim(result) <- dim(x)
  dimnames(result) <- dimnames(x)
  class(result) <- class(H)
  result
}

## Attempted sprintf.  doesn't align with "f", so can't use here.
## formatDec3 <- function(x, ..., displaydigits=decdigits, fmttype=c("e","f")) {
##   H <- formatHexInternal(x, ...)
##   precBits <- attr(H, "bindigits") + 1
##   decdigits <- ceiling(log(2^precBits, 10))
##   Hx <- scan(text=H, quiet=TRUE)
##   absHx <- abs(Hx)

##   Hx.range <- range(abs(Hx[Hx != 0]))
##   fieldwidth <- ceiling(diff(log(Hx.range, 10)))
##   fmttype <- match.arg(fmttype)
##   precision <- log(Hx.range[1], 10)
##   precision <- if (precision >= 0) 0 else -floor(precision)
##   digits <- max(decdigits, displaydigits)
##   switch(fmttype,
##          "e"=format <- paste("%+", as.character(digits), ".", as.character(digits), "e", sep=""),
##          "f"=format <- paste("%+", as.character(digits), ".", as.character(precision), "f", sep="")
##          )

##   result <- sprintf(format, Hx)

##   dim(result) <- dim(x)
##   dimnames(result) <- dimnames(x)
##   class(result) <- class(H)
##   result
## }

FindPrecBits <- function(x) {
      x1 <- strsplit(x[1],"")[[1]]
      plocation <- charmatch("p", x1)
      bxlocation <- charmatch(c("b","x"), x1)

}



scanBin <- function(x, precBits=stop("Must specify precBits.", call.=FALSE), scientific=TRUE) {
  class(x) <- NULL
  if (!scientific) {
    x <- gsub("_", "0", x)
    if (missing(precBits)) {
      ## TODO: why warning?  Rather just make this the explicit default ?
      ## rmh: no.  we need to count the number of actual bits, and do it before converting "_" to "0".
      precBits <- Rmpfr::mpfr_default_prec()
      warning("Default   precBits = ", precBits)
    }
  }
  if (is.null(precBits)) {
    x1 <- strsplit(x[1],"")[[1]]
    plocation <- charmatch("p", x1)
    precBits <- nchar(paste0(substring(x[1], 4, 4),
                             substring(x[1], 6, plocation-1)))
  }
  mpfr(x, base=2, precBits=precBits)
}

scanHex <- function(x, precBits=NULL) {
  class(x) <- NULL
  if (is.null(precBits)) {
    x1 <- strsplit(x[1],"")[[1]]
    plocation <- charmatch("p", x1)
    precBits <- 1 + nchar(substring(x[1], 6, plocation-1))*4
  }
  mpfr(x, base=16, precBits=precBits)
}



if (FALSE) {
## We can't detect the precision from the character string.
  ## base must be explicit as an argument
  mpfr("0b101.1", base=16)  ## precision bug in mpfr   ## this is valid hex without the base=16 argument
  mpfr("0b101.1", base=2)   ## precision bug in mpfr
  mpfr("101.1", base=16)                               ## this is valid hex without the base=16 argument
  mpfr("101.1", base=2)
}

if (FALSE) {
  ## We could do this.  I think it is not worth the trouble.
  X16 <- "0b01.1"
  class(X16) <- c("noquote")
  attr(X16, "display") <- "hex"
  X16

  X2 <- "0b01.1"
  class(X2) <- c("noquote")
  attr(X2, "display") <- "bin"
  X2

  ## and in that case we need
  print.hex <- print.bin <- function(x, ...) {
    x2 <- x
    attr(x2, display) <- NULL
    print(x2)
    invisible(x)
  }
  ## I donbt this is bullet-proof.  I am not recommending this option.
}
