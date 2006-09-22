"panel.interaction2wt" <-
function(x, y, subscripts, 
         responselab, trace.values, factor.levels,
         fun=mean, ...,
         key.in=NULL ## list of key arguments
         ) {
  tpg <- trellis.par.get("superpose.line")
  if.R(r={
    ## This loop is needed because
    ##    console usage has i=2,
    ##    Rcmdr script window has i=5,
    ##    Rcmdr justDoIt has i=20
    for (i in seq(2, length=30)) {
      sf2 <- sys.frame(i)
      column.tmp <- try(get("column", pos=sf2), silent=TRUE)
      if (class(column.tmp) != "try-error") break
    }
    if (class(column.tmp) == "try-error")
      stop("panel.interaction2wt is too deeply nested in the system stack.")
    row <- get("row", pos=sf2)
    column <- get("column", pos=sf2)
    cols.per.page <- get("cols.per.page", pos=sf2)
    rows.per.page <- get("rows.per.page", pos=sf2)
  },
       s={})
  if (column==1) {

    trace.factor <- dimnames(factor.levels)[[1]][row]
    trace.levels <- factor.levels[[trace.factor, "levels"]]
    n.levels <- min(length(tpg$col), length(trace.levels))
    key.list <- list(title=trace.factor,
                     cex.title=1,
                     corner=c(.5,.5), border=TRUE,
                     text=list(text=trace.levels, cex=.8),
                     lines=list(
                       col=tpg$col[1:n.levels],
                       lty=tpg$lty[1:n.levels],
                       lwd=tpg$lwd[1:n.levels]))
    key.list[names(key.in)] <- key.in
    if.R(r=draw.key(key.list, draw=TRUE),
         s=do.call(key, key.list))
    return()
  }

  ## column > 1



  cell <- if.R(r=panel.number(),
               s=get("cell", frame=sys.parent()))
  which.cell <- if.R(r=as.vector(
                       matrix(seq(get("plots.per.page", pos=sf2)),
                              nrow=cols.per.page)
                              [-1,]),
                     s=get("which.cell", frame=sys.parent()))
  this.cell <- match(cell, which.cell)
  these.labels <- 
    if.R(r=
         c(trace.factor=dimnames(factor.levels)[[1]][row],
           x.factor=dimnames(factor.levels)[[1]][column-1])
         ,s=
         get("panel.labels", frame=sys.parent())[this.cell,])
  
  trace.levels <- factor.levels[[these.labels["trace.factor"], "levels"]]
  x.levels <- factor.levels[[these.labels["x.factor"], "position"]]  ## change of name ok
  
  if (these.labels["x.factor"] == these.labels["trace.factor"]) {
    box.par <- list(box.dot=trellis.par.get("box.dot"),
                    box.rectangle=trellis.par.get("box.rectangle"),
                    box.umbrella=trellis.par.get("box.umbrella"))
    box.col <- lapply(box.par, function(x) list(col=x$col))
   if.R(r={
     tpg.col <- rep(tpg$col, length=length(trace.levels))
     tpg.lty <- rep(tpg$lty, length=length(trace.levels))
     for (i in 1:length(trace.levels)) {
       for (j in seq(along=box.col))
         box.col[[j]]$col <- tpg.col[i]
       box.col$box.rectangle$lty <- tpg.lty[i]
       trellis.par.set(box.col)
       
       position <- factor.levels[[these.labels["x.factor"], "position"]]
       x.position <- as.position(x)
       yx <- y[x.position == position[i]]
       ix <- rep(position[i], length(yx))
       panel.bwplot(ix, yx, horizontal = FALSE)
     }
     trellis.par.set(lapply(box.par, function(x) list(col=x$col)))
   }
        ,s=
        panel.bwplot.intermediate.hh(as.position(x), y, transpose=TRUE,
                                     at=position(x), ...)
        )
  }
  else {
    tab <- tapply(y, list(x, trace.values[subscripts]), fun)
    su.x <- sort(unique(x))
    tpg2 <- trellis.par.get("superpose.line")
    tpg2.col <- rep(tpg2$col, length=ncol(tab))
    tpg2.lty <- rep(tpg2$lty, length=ncol(tab))
    if.R(r=for (j in 1:ncol(tab))
         llines(x=su.x, y=tab[,j], col=tpg2.col[j], lty=tpg2.lty[j]),
         s=
    matlines(su.x, tab, col=tpg$col, lty=tpg$lty))
  }

  if (row==1 && column>1) {
    x.center <- mean(current.panel.limits()$xlim)
    ylim <- current.panel.limits()$ylim
    y.bottom <- ylim[1]-.2*diff(ylim)
    old.clip <- trellis.par.set(clip=list(panel="off"))
    panel.text(labels=these.labels["x.factor"],
               x=x.center,
               y=y.bottom, srt=0, adj=1, cex=.9)
    trellis.par.set(old.clip)
  }

  if (column==cols.per.page) {
    y.center <- mean(current.panel.limits()$ylim)
    xlim <- current.panel.limits()$xlim
    x.right <- xlim[2]+.2*diff(xlim)
    old.clip <- trellis.par.set(clip=list(panel="off"))
    panel.text(labels=responselab,
               x=x.right,
               y=y.center, srt=0, adj=1, cex=.9)
    trellis.par.set(old.clip)
    ## mtext(responselab, side=4, line=3, at=y.center, srt=0, adj=1, cex=.9)
  }
}
