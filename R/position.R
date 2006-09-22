"position<-" <- function(x, value) {
  x <- as.factor(x)
  if (length(levels(x)) != length(value))
    stop("length(levels(x)) != length(value)")
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
      x <- as.numeric(ordered(lev.x, lev.x))  ## ordered with non-numeric levels
  }
  else
    x <- seq(along=lev.x) ## factor
  x
}



as.position <- function(x) {
  if (is.numeric(x))
    x
  else
    position(x)[x]
}
