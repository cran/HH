"position<-" <- function(x, value) {
  x <- as.ordered(x)
  if (length(levels(x)) != length(value))
    stop("length(levels(x)) != length(value)")
  if (!is.positioned(x))
    if.R(r=class(x) <- c("positioned", class(x)),
         s=oldClass(x) <- c("positioned", oldClass(x)))
  attr(x, "position") <- value
  x
}


position <- function(x) {
  px <- attr(x, "position")
  if (!is.null(px)) return(px) ## "position" exists
  if (is.numeric(x)) return(x) ## numeric
  if (!inherits(x, "factor")) stop("x must be either numeric or factor.")
  lev.x <- levels(x)
  if (inherits(x, "ordered")) {
    on.exit(options(old.warn))
    old.warn <- options(warn=-1)
    if (!any(is.na(as.numeric(lev.x))))
      x <- as.numeric(lev.x)  ## ordered with numeric levels
    else
      x <- seq(along=lev.x)  ## ordered with non-numeric levels
  }
  else
    x <- seq(along=lev.x) ## factor
  x
}

unpositioned <- function(x, ...) {
  if (!is.positioned(x)) return(x)
  oldClass(x) <- oldClass(x)["positioned" != oldClass(x)]
  attr(x, "position") <- NULL
  x
}

is.numeric.positioned <- function(x, ...) { ## S-Plus
  ## S-Plus requires this and it doesn't get dispatched.
  ## It must be called explicitly
  if (is.positioned(x)) FALSE
  else
    NextMethod("is.numeric")
}

as.numeric.positioned <- function(x, ...) { ## S-Plus
  if (is.numeric.positioned(x)) ## S-Plus doesn't dispatch.
    x
  else
    position(x)[x]
}

as.double.positioned <- function(x, ...) { ## R
  if (is.numeric(x))
    x
  else
    position(x)[x]
}

"[.positioned" <- function (x, ..., drop = FALSE) {
  if.R(s={drop=FALSE}, r={}) ## S-Plus has an argument matching problem
  y <- NextMethod("[")
  if (drop) position(y) <- position(x)[!is.na(match(levels(x), levels(y)))]
  else  position(y) <- position(x)
  y
}

as.positioned <- function(x) {
  if (is.positioned(x)) x
  else {
    z <- as.ordered(x)
    position(z) <- position(z)
    z
  }
}

is.positioned <- function(x)
  inherits(x, "positioned")

positioned <- function(x, ..., value) {
  x <- ordered(x, ...)
  position(x) <-
    if (!missing(value)) value
    else position(x)
  x
}

print.positioned <- function(x, ...) {
  xx <- x
  attr(x, "position") <- NULL
  oldClass(x) <- oldClass(x)[-1]
  NextMethod("print")
  cat("position:", paste(position(xx), collapse = " < "), "\n")
  invisible(xx)
}

unique.positioned <- function(x, incomparables = FALSE, ...) {
  y <- unique(unpositioned(x), incomparables = FALSE, ...)
  position(y) <- position(x)
  y
}

## if.R(s=
     setOldClass(c("positioned", "ordered", "factor"))
##     ,
##     r={})
