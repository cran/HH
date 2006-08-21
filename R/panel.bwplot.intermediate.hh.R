"panel.bwplot.intermediate.hh" <-
function (x, y, box.ratio = 1, horizontal = TRUE, pch = box.dot$pch, 
    col = box.dot$col, cex = box.dot$cex, fill = box.rectangle$fill, 
    levels.fos = NULL, at, ...) 
{
  if (!is.null(list(...)$transpose)) horizontal <- !list(...)$transpose

  if (horizontal) fac.levels <- levels(y) else fac.levels <- levels(x)

  box.par <- list(box.dot=trellis.par.get("box.dot"),
                  box.rectangle=trellis.par.get("box.rectangle"),
                  box.umbrella=trellis.par.get("box.umbrella"))
  box.col <- lapply(box.par, function(x) list(col=x$col))
  tpg <- trellis.par.get("superpose.line")
  tpg.col <- rep(tpg$col, length=length(fac.levels))

  for (i in seq(along=fac.levels)) {
    for (j in seq(along=box.col))
      box.col[[j]]$col <- tpg.col[i]
    trellis.par.set(box.col)
    
    if (horizontal) {
      if (any(class(y)=="ordered")) {
        old.warn <- options(warn=-1)
        if (!any(is.na(as.numeric(levels(y)))))
          y.levels <- as.numeric(as.character((levels(y))))
        else
          y.levels <- seq(along=levels(y))
        options(old.warn)
      }
      else
        y.levels <- seq(along=levels(y))
      xy <- x[y.levels[y] == y.levels[i]]
      ii <- rep(i, length(xy))
      panel.bwplot.hh(xy, ii, horizontal = T,
                      at=if (is.numeric(y.levels)) y.levels[i] else i )
    }
    else {
      if (any(class(x)=="ordered")) {
        old.warn <- options(warn=-1)
        if (!any(is.na(as.numeric(levels(x)))))
          x.levels <- as.numeric(as.character((levels(x))))
        else
          x.levels <- seq(along=levels(x))
        options(old.warn)
      }
      else
        x.levels <- seq(along=levels(x))
      yx <- y[x.levels[x] == x.levels[i]]
      ii <- rep(i, length(yx))
      panel.bwplot.hh(ii, yx, horizontal = F,
                      at=if (is.numeric(x.levels)) x.levels[i] else i )
    }
  } 
}

