## In S-Plus
## HH revision of panel.pairs to add
##    1. the subpanel.scales argument
##    2. the panel.cex argument

## In R, a placeholder that tells the user to use panel.pairs.

panel.pairs.hh <-
  function(x, y, z, subscripts, pscales, subpanel = panel.splom,
           varnames = dimnames(x)[[2]], ...,
           subpanel.scales, panel.cex=par()$cex) {
  ## coordinate system already is 0,1 set up by the artificial x & y 
  shrink <- 0.04
  ## fraction to shrink data away from each edge
  x <- z[subscripts,  , drop = F]
  p <- ncol(x)
  x <- x/p/(1 + 2 * shrink)
  if(is.null(varnames))
    varnames <- paste("[, ", seq(length = p), "]", sep = "")
  ticks.offsets <- seq(0, 1, length = p + 1)
  abline(h = ticks.offsets, v = ticks.offsets)
  ## separating grid
  offsets <- ticks.offsets + shrink/p
  has.subscripts <- any(names(subpanel) == "subscripts")
  e <- 0.03/p
  ## tick length
  if (missing(subpanel.scales)) {
    cex <- (2.5/p) * (min(par("fin"))/8.5)
  } else {
    cex <- subpanel.scales$cex
  }
  ## tick labels cex
  do.scales <- is.list(pscales)
  for(j in 1:p) {
    for(i in 1:p) {
      if(i == j) {
        d <- ticks.offsets[i]
        if(do.scales) {
          labels <- pscales[[i]]$labels
          at <- (pscales[[i]]$at/(1 + 2 * shrink) + shrink)/p
          n <- length(at)
          n1 <- ceiling(n/2)
          n2 <- n1 + (n1 != (n + 1)/2)
          low <- at[1:n1]
          labels.low <- labels[1:n1]
          zl <- rep(0, length(low))
          high <- at[n2:n]
          labels.high <- labels[n2:n]
          zh <- rep(0, length(high))
          srt <- if(max(nchar(labels)) > 5 && length(labels) > 3) 90 else 0
          x1 <- c(d + low, d + high, d + zh, d + 1/p - e + zl)
          y1 <- c(d + zl, d + 1/p - e + zh, d + high, d + low)
          x2 <- c(d + low, d + high, d + e + zh, d + 1/p + zl)
          y2 <- c(d + e + zl, d + 1/p + zh, d + high, d + low)
          segments(x1, y1, x2, y2)
          text(d + low, d + 2 * e + zl, labels.low, adj = if(srt == 0) 0.5 else 0,
               srt = srt, cex = cex)
          text(d + high, d + 1/p - 3 * e + zh, labels.high, adj = if(srt == 0) 0.5
          else 1, srt = srt, cex = cex)
          text(d + 2 * e + zh, d + high, labels.high, adj = 0, cex = cex)
          text(d + 1/p - 2 * e + zl, d + low, labels.low, adj = 1, cex = cex)
        }
        text(0.5/p + d, 0.5/p + d, varnames[i], cex=panel.cex)
        next
      }
      else if(has.subscripts)
        subpanel(x[, j] + offsets[j], x[, i] + offsets[i], subscripts = subscripts, ...)
      else subpanel(x[, j] + offsets[j], x[, i] + offsets[i], ...)
    }
  }
}


if.R(r=
     panel.pairs.hh <-
     function(x, y, z, subscripts, pscales, subpanel = panel.splom,
              varnames = dimnames(x)[[2]], ...,
              subpanel.scales, panel.cex=par()$cex) {
       stop("Function 'panel.pairs.hh' doesn't work in R.  Use 'panel.pairs'.")
     }
     ,s={})
