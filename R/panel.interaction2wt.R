"panel.interaction2wt" <-
function(x, y, subscripts, 
         responselab, trace.values,
         factor.levels, factor.position,
         fun=mean,
         se,
         ...,
         box.ratio,
         simple=FALSE,
         simple.offset,
         simple.scale,
         data.x,
         col.by.row=TRUE,
         key.in=NULL ## list of key arguments
         ) {
  if.R(r={
    tcL <- trellis.currentLayout()
    rows.per.page <- dim(tcL)[1]
    cols.per.page <- dim(tcL)[2]
    cell <- panel.number()
    row.panel <- row(tcL)[tcL==cell]
    column.panel <- col(tcL)[tcL==cell]

    these.labels <- c(trace.factor=names(factor.levels)[row.panel],
                      x.factor=names(factor.levels)[column.panel])
  },s={
    rows.per.page <- length(factor.levels)
    cols.per.page <- length(factor.levels)
    cell <- get("cell", frame=sys.parent())

    tcL <- matrix(seq(length=rows.per.page*cols.per.page),
                  rows.per.page, cols.per.page,
                  byrow=TRUE)
    
    these.labels <- get("panel.labels", frame=sys.parent())[cell,]

    row.panel <- match(these.labels["trace.factor"], names(factor.levels))
    column.panel <- match(these.labels["x.factor"], names(factor.levels))
  }
       )
  
  trace.name <- these.labels["trace.factor"]
  x.name <- these.labels["x.factor"]
  trace.levels <- factor.levels[[trace.name]]
  x.position <- factor.position[[x.name]]
  x.levels <- factor.levels[[x.name]]

  tpg <- trellis.par.get("superpose.line")
  tpg.col <- rep(tpg$col, length=length(trace.levels))
  tpg.lty <- rep(tpg$lty, length=length(trace.levels))
  tpg.lwd <- rep(tpg$lwd, length=length(trace.levels))

  ## panels
  if (trace.name ==  x.name) {## main diagonal
    if (simple && cols.per.page==2) { ## simple effects
      other.name <- names(factor.levels)[names(factor.levels) != x.name]
      other.factor <- data.x[, other.name]
      x.factor <- data.x[, x.name]
      ioh.list <- list(x.factor, other.factor)
      if (!missing(simple.offset))
        ioh.list$b.offset <- simple.offset[[other.name]]
      if (!missing(simple.scale))
        ioh.list$b.scale=simple.scale[[other.name]]
      x.simple <- do.call("interaction.positioned", ioh.list)
## recover()
      tpg.col.simple <-
        if (col.by.row)
          tpg.col
        else
          rep(tpg$col, length=length(levels(other.factor)))
      
      col.subscripts <-
        if (col.by.row)
          rep(seq(length(x.levels)), each=length(levels(other.factor)))
        else
          rep(seq(length(levels(other.factor))), length(x.levels))

      if.R(r=
           panel.bwplot.intermediate.hh(x.simple, y,
                                        horizontal=FALSE,
                                        col=tpg.col.simple[col.subscripts],
                                        box.ratio=box.ratio,
                                        ...)
           ,s=
           panel.bwplot.intermediate.hh(as.numeric(x.simple), y,
                                        horizontal=FALSE,
                                        col=tpg.col.simple[col.subscripts],
                                        box.ratio=box.ratio,
                                        ...)
           )
    }
    else { ## marginal main effects
      x.factor <- data.x[, x.name]
      if.R(r=
           panel.bwplot.intermediate.hh(x.factor, y,
                                        horizontal=FALSE,
                                        box.ratio=box.ratio,
                                        ...)
           ,s=
           panel.bwplot.intermediate.hh(as.numeric(x.factor), y,
                                        horizontal=FALSE,
                                        box.ratio=box.ratio,
                                        ...)
           )
    }
  } 
  else { ## off-diagonal
    if (simple) {
      suff.data <-
        sufficient(data.frame(y, x, tt=trace.values[subscripts]),
                   y="y", c("x", "tt"))

      ioh.list <- list(suff.data$x, suff.data$tt)
      if (!missing(simple.offset))
        ioh.list$b.offset <- simple.offset[[trace.name]]
      if (!missing(simple.scale))
        ioh.list$b.scale=simple.scale[[trace.name]]
      x.simple <- do.call("interaction.positioned", ioh.list)

      if (missing(se) || (is.logical(se) && !se))
        panel.intxplot(y=suff.data$y,
                       x=x.simple,
                       subscripts=seq(length(suff.data$y)),
                       groups=suff.data$tt,
                       offset.use=FALSE,
                       rug.use=TRUE,
                       ...)
      else {
        if (is.logical(se))
          suff.data.se <- suff.data$sd/sqrt(suff.data$nobs)
        else {
          suff.data.se <- eval(se, local=suff.data)
        }
        panel.intxplot(y=suff.data$y,
                       x=x.simple,
                       subscripts=seq(length(suff.data$y)),
                       groups=suff.data$tt,
                       offset.use=FALSE,
                       rug.use=TRUE,
                       se=suff.data.se,
                       ...)
      }
    }
    else {
      tab <- tapply(y, list(x, trace.values[subscripts]), fun)
      su.x <- sort(unique(x))
      if.R(r={}, s=panel.lines <- lines)
      for (j in 1:ncol(tab))
        panel.lines(x=su.x, y=tab[,j], col=tpg.col[j], lty=tpg.lty[j])
    }
  }


  
  ## x labels
  if (row.panel==1) {
    if.R(r={},
         s={
           axis(1, at=x.position, labels=x.levels)
           mtext(x.name, side=1, line=2, at=mean(par()$usr[1:2]))
          })
}

  ## trace key
  if (column.panel==1) {

    key.list <- list(title=trace.name,
                     cex.title=1,
                     corner=c(.5,.5), border=TRUE,
                     text=list(text=trace.levels, cex=.8),
                     lines=list(col=tpg.col, lty=tpg.lty, lwd=tpg.lwd))
    key.list[names(key.in)] <- key.in
    if.R(r={},
         s={
           key.list$x <- par()$usr[1]-.6*diff(par()$usr[1:2])
           do.call("key", key.list)
         }
         )
  }


  ## y label
  if (column.panel==cols.per.page) {
    if.R(r={},
         s={
           mtext(responselab, side=4, line=3, at=mean(par()$usr[3:4]))
         }
         )
  }
}


axis.i2wt <-
  function(side, scales, ...)
{
  axis.options <- lattice.getOption("axis.options")

  tcL <- trellis.currentLayout()
  rows.per.page <- dim(tcL)[1]
  cols.per.page <- dim(tcL)[2]
  cell <- panel.number()
  row.panel <- row(tcL)[tcL==cell]
  column.panel <- col(tcL)[tcL==cell]
  
  if (side == "bottom")
    {
      if (row.panel == 1)
        {

          axis.options <- axis.options$bottom
          at2     <- axis.options$at2                     
          labels2 <- axis.options$labels2                 
          rot2    <- axis.options$rot2
          ticks2  <- axis.options$ticks2
          at3     <- axis.options$at3                     
          labels3 <- axis.options$labels3                 
          rot3    <- axis.options$rot3
          ticks3  <- axis.options$ticks3
          if (is.null(rot2)) rot2 <- 0
          if (is.null(rot3)) rot3 <- 0
          if (is.null(ticks2)) ticks2 <- !is.null(labels2)
          if (is.null(ticks3)) ticks3 <- FALSE

          axis.units <- lattice.getOption("axis.units")$outer$bottom
          
          ## space occupied by standard set of ticks
          tick.units <-
            ## if (is.null(scales$at)) unit(0, "lines")
            ## else
              with(axis.units$tick, unit(scales$tck * x, units))
          
          ## space occupied by standard set of labels, assume 1.2
          ## lines, but we have enough information to compute
          ## explicitly (see how 'lab.grob' is computed in layout.R)
          
          label.units <-
            if (is.null(scales$at) || !scales$at) unit(0, "lines")
            else unit(1.2, "lines")
          
          ## want second set of ticks to extend beyond this.
          
          if (!is.null(at2))
            at2i <-
              if (is.list(at2))
                at2[[column.panel]]
              else at2
          
          if (!is.null(labels2))
            labels2i <-
              if (is.list(labels2))
                labels2[[column.panel]]
              else labels2

          labels2.units <-
            if (!is.null(at2) && !is.null(labels2)) 
              heightDetails(textGrob(unlist(labels2), rot = rot2,
                                     x=seq(along=unlist(labels2)),
                                     just = if (rot2==90)
                                     c("right", "center")
                                     else c("center", "top")))
            else
              unit(0, "lines")

          if (ticks2 && !is.null(at2))
            grid.segments(x0 = unit(at2i, "native"),
                          x1 = unit(at2i, "native"),
                          y0 = unit(0, "npc"),
                          y1 = unit(0, "npc") - tick.units -
                          label.units)
          
          if (!is.null(at2) && !is.null(labels2))
            grid.text(labels2i, rot = rot2,
                      x = unit(at2i, "native"),
                      y = unit(0, "npc") - tick.units - label.units -
                      unit(1, "mm"),
                      just = if (rot2==90) c("right", "center") else c("center", "top"))
          
          ## other set of labels (without ticks)
          
          if (ticks3 && !is.null(at3))
            grid.segments(x0 = unit(at3, "native"),
                          x1 = unit(at3, "native"),
                          y0 = unit(0, "npc"),
                          y1 = unit(0, "npc") - tick.units -
                          label.units - unit(1, "mm"))

          if (!is.null(at3) && !is.null(labels3))
            grid.text(labels3, rot=rot3,
                      x = unit(at3, "native"),
                      y = unit(0, "npc") - tick.units - label.units -
                      labels2.units - unit(2, "mm"),
                      just = c("center", "top"))
          if (is.null(at3) && !is.null(labels3))
            grid.text(labels3[column.panel], rot=rot3,
                      y = unit(0, "npc") - tick.units - label.units -
                      labels2.units - unit(2, "mm"),
                      just = c("center", "top"))
        }
    }
  
  if (side == "right")
    {
      if (column.panel == cols.per.page)
        {
          axis.options <- axis.options$right
          at2     <- axis.options$at2                     
          labels2 <- axis.options$labels2                 
          rot2    <- axis.options$rot2
          ticks2  <- axis.options$ticks2
          at3     <- axis.options$at3                     
          labels3 <- axis.options$labels3                 
          rot3    <- axis.options$rot3
          ticks3  <- axis.options$ticks3
          if (is.null(rot2)) rot2 <- 0
          if (is.null(rot3)) rot3 <- 0
          if (is.null(ticks2)) ticks2 <- !is.null(labels2)
          if (is.null(ticks3)) ticks3 <- FALSE

          axis.units <- lattice.getOption("axis.units")$outer$right
  
          ## space occupied by standard set of ticks
          tick.units <-
            ## if (is.null(scales$at)) unit(0, "lines")
            ## else
              with(axis.units$tick, unit(scales$tck * x, units))
          
          ## space occupied by standard set of labels, assume 1.2
          ## lines, but we have enough information to compute
          ## explicitly (see how 'lab.grob' is computed in layout.R)
          
          label.units <-
            if (is.null(scales$at) || !scales$at) unit(0, "lines")
            else unit(1.2, "lines")
          
          ## want second set of ticks to extend beyond this.
          
          if (!is.null(at2))
            at2i <-
              if (is.list(at2))
                at2[[row.panel]]
              else at2
          
          if (!is.null(labels2))
            labels2i <-
              if (is.list(labels2))
                labels2[[row.panel]]
              else labels2

          labels2.units <-
            if (## !is.null(at2) &&  ## Not the same as bottom
                !is.null(labels2))
              widthDetails(textGrob(unlist(labels2), rot = rot2,
                                    y=seq(along=unlist(labels2)),
                                    just = if (rot2==90)
                                    c("center", "center")
                                    else c("left", "center")))
            else
              unit(0, "lines")

          right.margin <- unit(current.panel.limits()$xlim[2],"native")
          if (FALSE)  ## (ticks2 && !is.null(at2))  ## not the same
            grid.segments(y0 = unit(at2i, "native"),
                          y1 = unit(at2i, "native"),
                          x0 = right.margin,
                          x1 = right.margin + tick.units +
                          label.units)
          
          if (FALSE)  ## (!is.null(at2) && !is.null(labels2))  ## not the same
            grid.text(labels2i, rot = rot2,
                      y = unit(at2i, "native"),
                      x = right.margin + tick.units + label.units +
                      unit(1, "mm"),
                      just = if (rot2==90) c("center", "center") else c("left", "center"))
          
          ## other set of labels (without ticks)
          
          if (ticks3 && !is.null(at3))
            grid.segments(y0 = unit(at3, "native"),
                          y1 = unit(at3, "native"),
                          x0 = right.margin,
                          x1 = right.margin + tick.units +
                          label.units + unit(1, "mm"))

          if (!is.null(at3) && !is.null(labels3))
            grid.text(labels3, rot=rot3,
                      y = unit(at3, "native"),
                      x = right.margin + tick.units + label.units +
                      labels2.units + unit(2, "mm"),
                      just = c("left", "top"))
          if (is.null(at3) && !is.null(labels3))
            grid.text(labels3[row.panel], rot=rot3,
                      x = right.margin + tick.units + label.units +
                      labels2.units + unit(2, "mm"),
                      just = c("left", "top"))
        }
    }
  axis.default(side = side, scales = scales, ...)
}


legendGrob2wt <-   function(...) ## ...is key1, key2, etc
{
  keys <- rev(list(...))
  
  ## rev is needed because grid and lattice count rows differently (but
  ## only when as.table = FALSE -- you will need to deal with this
  ## somehow, possibly by capturing as.table in your wrapper)
  
  nkeys <- length(keys)
  key.widths <-
    lapply(keys,
           function(key) unit(1, "grobwidth", data = list(key)))
  key.layout <-
    grid.layout(nrow = nkeys, ncol = 1,
                heights = unit(1, "null"),
                widths = do.call(max, key.widths),
                respect = TRUE)
  key.gf <- frameGrob(layout = key.layout)
  for (i in seq_len(nkeys))
    {
      key.gf <- placeGrob(key.gf, keys[[i]], row = i, col = 1)
    }
  key.gf
}

## source("~/HH-R.package/HH/R/interaction2wt.R")
## source("~/HH-R.package/HH/R/panel.interaction2wt.R")
## trace(axis.i2wt, exit=recover)
