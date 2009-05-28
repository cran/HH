"panel.bwplot.intermediate.hh" <-
function (x, y,
          horizontal = TRUE,
          transpose=!horizontal,
          pch, 
          col,
          at=if (horizontal) levels(as.factor(y)) else levels(as.factor(x)),  ## S-Plus only
          ...
          )
{
##browser()
  if (missing(horizontal) && !missing(transpose))
    horizontal <- !transpose

  fac.levels <- if.R(r=if (horizontal) levels(y) else levels(x),
                     s=at)
  box.par <- list(box.dot=trellis.par.get("box.dot"),
                  box.rectangle=trellis.par.get("box.rectangle"),
                  box.umbrella=trellis.par.get("box.umbrella"),
                  plot.symbol=trellis.par.get("plot.symbol"))
  tpg <- trellis.par.get("superpose.line")
  tpg.col <- rep(tpg$col, length=length(fac.levels))
  if (!missing(col)) tpg.col <- rep(col, length=length(fac.levels))

  for (i in seq(along=fac.levels)) {
    if (!missing(pch)) {
      box.par$box.dot$pch <- pch[i]
      box.par$plot.symbol$pch <- pch[i]
    }
    for (j in names(box.par)) {
##browser()
      box.par[[j]]$col <- tpg.col[i]
      trellis.par.set(j, box.par[[j]])
    }
    
    if (horizontal) {
        ii <- as.position(y[y == fac.levels[i]])
        xy <- x[y == fac.levels[i]]
      if.R({
        r=panel.bwplot(xy, ii, horizontal=horizontal, ...)
      },s={
        if (is.numeric(at)) {
          ii <- rep(fac.levels[i], sum(y==i))
          xy <- x[y == i]
        }
        panel.bwplott(xy, ii, transpose=transpose, ...)
      })
    }
      else {
          yx <- y[x == fac.levels[i]]
          ii <- as.position(x[x == fac.levels[i]])
        if.R(r={
          panel.bwplot(ii, yx, horizontal=horizontal, ...)
        },s={
          if (is.numeric(at)) {
            ii <- rep(fac.levels[i], sum(x==i))
            yx <- y[x == i]
          }
          panel.bwplott(ii, yx, transpose=transpose, ...)
        })
      }
  }
}

##source("c:/HOME/rmh/HH-R.package/HH/R/panel.bwplot.intermediate.hh.R")
