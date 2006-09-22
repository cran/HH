"panel.bwplot.intermediate.hh" <-
function (x, y, horizontal = TRUE, pch = box.dot$pch, 
    col = box.dot$col, ...) 
{
  if (!is.null(list(...)$transpose)) horizontal <- !list(...)$transpose

  if (horizontal) fac.levels <- levels(y) else fac.levels <- levels(x)

  box.dot <- trellis.par.get("box.dot")
  box.par <- list(box.dot=box.dot,
                  box.rectangle=trellis.par.get("box.rectangle"),
                  box.umbrella=trellis.par.get("box.umbrella"))
  box.col <- lapply(box.par, function(x) list(col=x$col))
  tpg <- trellis.par.get("superpose.line")
  tpg.col <- rep(tpg$col, length=length(fac.levels))

  for (i in seq(along=fac.levels)) {
    if (!missing(pch)) box.col$box.dot$pch <- pch[i]
    for (j in seq(along=box.col))
      box.col[[j]]$col <- tpg.col[i]
    trellis.par.set(box.col)
    
    if (horizontal) {
      y.levels <- position(y)
      xy <- x[y.levels[y] == y.levels[i]]
      ii <- rep(y.levels[i], length(xy))
      panel.bwplot(xy, ii, horizontal = TRUE, ...)
    }
    else {
      x.levels <- position(x)
      yx <- y[x.levels[x] == x.levels[i]]
      ii <- rep(x.levels[i], length(yx))
      panel.bwplot(ii, yx, horizontal = FALSE, ...)
    }
  } 
}
