"panel.bwplot.hh" <-
function (x, y, box.ratio = 1, horizontal = TRUE, pch = box.dot$pch, 
    col = box.dot$col, cex = box.dot$cex, fill = box.rectangle$fill, 
    varwidth = FALSE, levels.fos = NULL, coef = 1.5, at, ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")
    if (horizontal) {
        maxn <- max(by(x, y, length))
        yscale <- current.viewport()$yscale
        if (is.null(levels.fos)) 
            levels.fos <- floor(yscale[2]) - ceiling(yscale[1]) + 
                1
        lower <- ceiling(yscale[1])
        height <- box.ratio/(1 + box.ratio)
        xscale <- current.viewport()$xscale
        if (levels.fos > 0) 
            for (i in 1:levels.fos) {
                yval <- i
                stats <- boxplot.stats(x[y == yval], coef = coef)
                if (stats$n > 0) {
                  pushViewport(viewport(y = unit(if(missing(at)) yval else at, "native"), 
                    height = unit((if (varwidth) 
                      sqrt(stats$n/maxn)
                    else 1) * height, "native"), xscale = xscale))
                  r.x <- (stats$stats[2] + stats$stats[4])/2
                  r.w <- stats$stats[4] - stats$stats[2]
                  grid.rect(x = unit(r.x, "native"), width = unit(r.w, 
                    "native"), gp = gpar(lwd = box.rectangle$lwd, 
                    lty = box.rectangle$lty, fill = fill, col = box.rectangle$col))
                  grid.lines(x = unit(stats$stats[1:2], "native"), 
                    y = unit(c(0.5, 0.5), "npc"), gp = gpar(col = box.umbrella$col, 
                      lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(x = unit(stats$stats[4:5], "native"), 
                    y = unit(c(0.5, 0.5), "npc"), gp = gpar(col = box.umbrella$col, 
                      lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(x = unit(rep(stats$stats[1], 2), 
                    "native"), y = unit(c(0, 1), "npc"), gp = gpar(col = box.umbrella$col, 
                    lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(x = unit(rep(stats$stats[5], 2), 
                    "native"), y = unit(c(0, 1), "npc"), gp = gpar(col = box.umbrella$col, 
                    lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.points(x = stats$stats[3], y = 0.5, pch = pch, 
                    size = unit(cex * 2.5, "mm"), gp = gpar(col = col, 
                      cex = cex))
                  if ((l <- length(stats$out)) > 0) 
                    grid.points(x = stats$out, y = rep(0.5, l), 
                      size = unit(plot.symbol$cex * 2.5, "mm"), 
                      pch = plot.symbol$pch, gp = gpar(col = plot.symbol$col, 
                        cex = plot.symbol$cex))
                  popViewport()
                }
            }
    }
    else {
        maxn <- max(by(y, x, length))
        xscale <- current.viewport()$xscale
        if (is.null(levels.fos)) 
            levels.fos <- floor(xscale[2]) - ceiling(xscale[1]) + 
                1
        lower <- ceiling(xscale[1])
        width <- box.ratio/(1 + box.ratio)
        yscale <- current.viewport()$yscale
        if (levels.fos > 0) 
            for (i in 1:levels.fos) {
                xval <- i
                stats <- boxplot.stats(y[x == xval], coef = coef)
                if (stats$n > 0) {
                  pushViewport(viewport(x = unit(if(missing(at)) xval else at, "native"), 
                    width = unit((if (varwidth) 
                      sqrt(stats$n/maxn)
                    else 1) * width, "native"), yscale = yscale))
                  r.x <- (stats$stats[2] + stats$stats[4])/2
                  r.w <- stats$stats[4] - stats$stats[2]
                  grid.rect(y = unit(r.x, "native"), height = unit(r.w, 
                    "native"), gp = gpar(lwd = box.rectangle$lwd, 
                    lty = box.rectangle$lty, fill = fill, col = box.rectangle$col))
                  grid.lines(y = unit(stats$stats[1:2], "native"), 
                    x = unit(c(0.5, 0.5), "npc"), gp = gpar(col = box.umbrella$col, 
                      lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(y = unit(stats$stats[4:5], "native"), 
                    x = unit(c(0.5, 0.5), "npc"), gp = gpar(col = box.umbrella$col, 
                      lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(y = unit(rep(stats$stats[1], 2), 
                    "native"), x = unit(c(0, 1), "npc"), gp = gpar(col = box.umbrella$col, 
                    lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.lines(y = unit(rep(stats$stats[5], 2), 
                    "native"), x = unit(c(0, 1), "npc"), gp = gpar(col = box.umbrella$col, 
                    lwd = box.umbrella$lwd, lty = box.umbrella$lty))
                  grid.points(y = stats$stats[3], x = 0.5, pch = pch, 
                    size = unit(cex * 2.5, "mm"), gp = gpar(col = col, 
                      cex = cex))
                  if ((l <- length(stats$out)) > 0) 
                    grid.points(y = stats$out, x = rep(0.5, l), 
                      size = unit(plot.symbol$cex * 2.5, "mm"), 
                      pch = plot.symbol$pch, gp = gpar(col = plot.symbol$col, 
                        cex = plot.symbol$cex))
                  popViewport()
                }
            }
    }
}

