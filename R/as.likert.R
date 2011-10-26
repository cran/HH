as.MatrixList <- function(x, ...)
  UseMethod("as.MatrixList")

as.MatrixList.array <- function(x, ...) {
  ldx <- length(dim(x))
  xa <- lapply(apply(x, 3:ldx, function(x) list(x)), `[[`, 1)
  dim(xa) <- dim(x)[-(1:2)]
  dimnames(xa) <- dimnames(x)[-(1:2)]
  if (is.null(names(xa))) { ## getting here means ldx > 3
    nxa <- outer(dimnames(x)[[3]], dimnames(x)[[4]], "paste", sep=".")
    if (ldx >= 5) {
      for (i in 5:ldx)
      nxa <- outer(nxa, dimnames(x)[[i]],, "paste", sep=".")
    }
    names(xa) <- nxa
  }
  xa
}

is.likert <- function(x) inherits(x, "likert")

as.likert <- function(x, ...)
  UseMethod("as.likert")

as.likert.likert <- function(x, ...) return(x)
  
as.likert.data.frame <- function(x, ...) {
  as.likert(data.matrix(x), ...)
}

as.likert.formula <- function(x, ...) {
  data <- list(...)$data
  stop("use one of the idioms
    as.likert(tapply( <appropriate arguments> ))
    as.likert(table( <appropriate arguments> ))",
       call.=FALSE)
  ## data(apple)
  ## as.likert(tapply(apple$pre, apple[,1:2], c))
  ## as.likert(table(apple[,1:2]))
  ##
  ## eventually I will need to automate this:
  ##    data[,deparse(x[[2]])]), data[, x[[3]]]
  ## where you have to manually unpack x[[3]].
}


as.likert.ftable <- function(x, ...) {
  as.likert(as.table(x, ...))
}

as.likert.table <- function(x, ...) {
  as.likert.matrix(x, ...)
}
 
as.likert.matrix <- function(x, rowlabel=NULL, collabel=NULL, ...) {

  is.even <- function(x) x%%2 == 0

  if (is.null(dimnames(x))) dimnames(x) <- list(1:nrow(x), NULL)
  if (is.null(dimnames(x)[[1]])) dimnames(x)[[1]] <- 1:nrow(x)
  
  levels <- dimnames(x)[[2]]
  if (is.null(levels)) {
    if (ncol(x) == 1) levels <- ""
    else levels <- LETTERS[1:ncol(x)]
    dimnames(x)[[2]] <- levels
  }
  
  if (!is.null(rowlabel)) names(dimnames(x))[1] <- rowlabel
  if (!is.null(collabel)) names(dimnames(x))[2] <- collabel
  x <- x[nrow(x):1,, drop=FALSE]
  
  nc <- ncol(x)
  if(is.even(nc)) {
    ind.neg <- 1:(nc/2)
    ind.pos <- (nc/2+1):nc
    x[,rev(ind.neg)] <- -x[,ind.neg, drop=FALSE]
    attr(x, "even.col") <- TRUE
  }
  else {
    if (nc > 1) {
      ind.neg <- floor(nc/2):1
      ind.zero <- ceiling(nc/2)
      ind.pos <- (ind.zero+1):nc
      x <- cbind(-x[,ind.zero, drop=FALSE]/2,
                 -x[,ind.neg, drop=FALSE],
                 x[,ind.zero, drop=FALSE]/2,
                 x[,ind.pos, drop=FALSE])
    } else {
      x <- cbind(-x/2, x/2)
    }
    attr(x, "even.col") <- FALSE
  }
  attr(x, "nlevels") <- nc
  attr(x, "levels") <- levels
  pos.cols <- if (nc > 1) -(1:(ncol(x)/2)) else 1
  attr(x, "positive.order") <- order(apply(x[, pos.cols, drop=FALSE], 1, sum))
  class(x) <- c("likert", class(x))
  x
}

as.likert.numeric <- function(x, ...) {
  x <- t(x)
  if (is.null(dimnames(x))) dimnames(x) <- list("", 1:length(x))
  if (is.null(dimnames(x)[[1]])) dimnames(x)[[1]] <- ""
  as.likert(x)
}

is.likert.capable <- function(x, ...) {
  is.numeric(x) ||
  is.table(x) ||
  inherits(x, "ftable") ||
  ("package:vcd" %in% search() && is.structable(x)) ||
  is.data.frame(x) ||
  is.listOfNamedMatrices(x)
}

## source("c:/HOME/rmh/HH-R.package/HH/R/as.likert.R")
