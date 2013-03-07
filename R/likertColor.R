ColorSet <- function(nc, ReferenceZero=NULL) {

  if (is.null(ReferenceZero)) ReferenceZero <- (nc+1)/2
  if (ReferenceZero < 1) ReferenceZero <- .5
  if (ReferenceZero > nc) ReferenceZero <- nc + .5

  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  maxcolorset <- if (is.wholenumber(ReferenceZero)) (-nc):nc else c(-rev(1:nc), 1:nc)
  start.position <- nc - ceiling(ReferenceZero) + 2
  maxcolorset[seq(start.position, length=nc)]
}


likertColorBrewer <- function(nc, ReferenceZero=NULL,
                        BrewerPaletteName="RdBu", middle.color="gray90") {
  colorset <- ColorSet(nc, ReferenceZero)
  ncolors <- max(abs(colorset))*2 + (0 %in% colorset)
  oneN2 <- (1:max(abs(colorset)))
  which <- c(-rev(oneN2), 0[0 %in% colorset], oneN2) %in% colorset
  brewer.pal.likert(ncolors, BrewerPaletteName, middle.color)[which]
}


likertColor <- function(nc, ReferenceZero=NULL, ...) {
  colorset <- ColorSet(nc, ReferenceZero)
  ncolors <- max(abs(colorset))*2 + (0 %in% colorset)
  oneN2 <- (1:max(abs(colorset)))
  which <- c(-rev(oneN2), 0[0 %in% colorset], oneN2) %in% colorset
  if (nc == 1 && (is.null(ReferenceZero) || ReferenceZero==1))
      diverge_hcl(3)[2]
  else
    rev(diverge_hcl(ncolors))[which]
}


brewer.pal.likert <- function(n, name,  middle.color) {

  is.odd <- function(x)  x%%2 == 1

  palette <-
    if (n <= 2) {
      bp <- brewer.pal(n=3, name=name)
      if (n==1) bp[2] else bp[-2]
    }
    else {
      if (n <= 11)
        brewer.pal(n=n, name=name)
      else {
        if (is.odd(n))
          colorRampPalette(brewer.pal(n=11, name=name))(n)
        else
          colorRampPalette(brewer.pal(n=10, name=name))(n)
      }
    }
  if (is.odd(n) && !missing(middle.color)) {
    middle <- (n %/% 2) + 1
    palette[middle] <- middle.color
  }
  palette
}

