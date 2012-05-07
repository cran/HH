interaction2wt <- function(x, ...)
  UseMethod("interaction2wt")

"interaction2wt.formula" <-
function(x, data=sys.parent(), responselab,
			    ...) {
  if.R(
       r={
         do.formula.trellis <- NA ## make R-2.6.0dev happy
         dft <- do.formula.trellis.xysplom(x, data=data)
         y.in <- dft$y[[1]]
         x.in <- dft$x
         if (missing(responselab)) responselab <- names(dft$y)
       },
       s={
         dft <- do.formula.trellis(x)
         y.in <- eval(dft$expr[[1]], local=data)
         x.in <- data[,dft$xlab,drop=FALSE]
         if (missing(responselab)) responselab <- dft$ylab
       })
  if (is.null(x.in) || is.null(y.in))
    stop("both x and y are needed in formula")
  interaction2wt.default(x=x.in, response.var=y.in,
                              responselab=responselab,
                              ...)
}

interaction2wt.default <-
  function(x, response.var,
           responselab=deparse(substitute(response.var)),
           responselab.expression = responselab,
           relation=list(x="sliced", y="same"),
           x.relation=relation$x, y.relation=relation$y,
           digits=3,
           x.between=if (label.as.interaction.formula) 0 else 1,
           y.between=if (label.as.interaction.formula) 0 else 1,
           between,
           cex=.75,
           ## rot=c(0,0),
           panel.input=panel.interaction2wt,
           strip.input=if (label.as.interaction.formula) strip.default
                       else strip.interaction2wt,
           par.strip.text.input=trellis.par.get()$add.text,  ##list(cex=.7)
           scales.additional,
           main.in=paste(responselab,
             ": main effects and 2-way interactions", sep=""),
           xlab="", ylab="",
           ylab.right=list(rep(responselab, k), rot=0),
           simple=FALSE,
           box.ratio=if (simple) .32 else 1,
           label.as.interaction.formula=TRUE,
           ...,
           main.cex,
           key.cex.title=trellis.par.get()$par.xlab.text$cex,
           key.cex.text=trellis.par.get()$axis.text$cex,
           factor.expressions=names.x
           ) {
    n <- nrow(x)
    k <- ncol(x)
    names.x <- names(x)
    names(names.x) <- names.x

  if (k<2) stop("interaction2wt requires at least two factors.")
  if (simple && k != 2) stop("Simple effects requires exactly two factors.")

  x.list <- x
  for (i in names(x)) {
    x[[i]] <- as.factor(x[[i]])
    x.list[[i]] <- as.numeric.positioned(x[[i]])
  }

  factor.levels <- lapply(x, levels)
  factor.position <- lapply(x, position)

##   scales.input <- list(x=list(
##                          relation=x.relation,
##                          alternating=FALSE,
##                          xaxt="n",  ## S-Plus
##                          draw=FALSE ## R
##                          ),
##                        y=list(relation=y.relation, alternating=2))
##
##
##     scales.input <- if.R(r={
##       list(x=list(
##              relation=x.relation,
##              alternating=FALSE,
##              draw=FALSE
##              ),
##            y=list(relation=y.relation, alternating=2))
##     }, s={
##       list(x=list(
##              relation=x.relation,
##              alternating=FALSE,
##              xaxt="n",
##              ),
##            y=list(relation=y.relation, alternating=2))
##     })
##
    xlist <- if.R(r={list(##relation=x.relation,
                          alternating=FALSE, draw=FALSE)},
                  s={list(relation=x.relation, alternating=FALSE, xaxt="n"  )})
    scales.input <- list(x=xlist,
                         y=list(relation=y.relation, alternating=3))


  if (!missing(scales.additional)) {
    scales.input$x[names(scales.additional$x)] <- scales.additional$x
    scales.input$y[names(scales.additional$y)] <- scales.additional$y
  }
  if.R(r={
##    scales.input$x$at <- NULL
##    scales.input$y$at <- NULL
##    scales.input$x$rot <- rot[1]
##    scales.input$y$rot <- rep(rot,2)[2]
  },
       s={})

  ccd <- data.frame(response.var=rep(response.var, length=n*k*k),
                    x.values    =unlist(rep(as.list(x.list), k)),
                    trace.values=unlist(rep(as.list(x.list), rep(k,k))),
                    x.factor    =factor(rep(rep(names.x, rep(n,k)), k),
                      levels=names.x),
                    trace.factor=factor(rep(    names.x, rep(n*k,   k)),
                      levels=names.x))
  if (label.as.interaction.formula) {
    ccd$x.trace <- interaction(ccd$x.factor, ccd$trace.factor)
    levels(ccd$x.trace) <- paste(responselab,
                                 outer(levels(ccd$x.factor),
                                       levels(ccd$trace.factor),
                                       FUN=paste,
                                       sep=" | "),
                                 sep=" ~ ")
    formula <- response.var ~ x.values | x.trace
  }
  else
    formula <- response.var ~ x.values | x.factor * trace.factor

  if (!missing(main.cex)) {
    main.in <- as.list(main.in)
    main.in$cex <- main.cex
  }

  xyplot.list <-
    list(formula,
         data=ccd,
         responselab=responselab,
         responselab.expression=responselab.expression,
         trace.values=ccd$trace.values,
         factor.levels=factor.levels,
         factor.position=factor.position,
         between=if (missing(between))
         list(x=x.between, y=y.between)
         else
         between,
         scales=scales.input,
         xaxs="e",
         prepanel=function(x,y) list(xlim=range(x)+c(-1,1)*.1*range(x)),
         panel=panel.input,
         strip=strip.input,
         par.strip.text=par.strip.text.input,
         layout=c(k, k),
         main=main.in,
         xlab="", ylab="",
         cex=cex, las=1, aspect=1,
         simple=simple,
         data.x=x,
         box.ratio=box.ratio,
         ylab.right=ylab.right,
         ...)
  if.R(r={
    ## cpy <- range(ccd$response.var)
    ## pcpy <- pretty(cpy)
    ## pcpy <- pcpy[(cpy[1] <= pcpy) & (pcpy <= cpy[2])]
## recover()

    rot <- xyplot.list$scales$rot
    if (is.null(rot)) rot <- xyplot.list$scales$x$rot
    if (is.null(rot)) rot <- 0

    cex <- xyplot.list$scales$cex
    if (is.null(cex)) cex <- xyplot.list$scales$x$cex
    if (is.null(cex)) cex <- trellis.par.get()$axis.text$cex
    
    lattice.options <-
      list(axis.options=list(
             bottom=list(
               at2=factor.position,
               labels2=factor.levels,
               rot2=rot[1],
               cex2=cex,
               labels3=factor.expressions)##, ## levels(ccd$x.factor)),
             ## right=list(
             ##   at2=pcpy,
             ##   labels2=pcpy,
             ##   rot2=rot[2],
             ##   labels3=rep(responselab.expression, k)
             ##   )
             ),
           layout.heights=list(axis.xlab.padding=list(x=15, units="mm")) ##,
           ## layout.widths=list(right.padding=list(x=13, units="mm"))
           )

    if (length(xyplot.list$lattice.options) == 0)
      xyplot.list$lattice.options <- list()
    xyplot.list$lattice.options[names(lattice.options)] <- lattice.options

    keys <- vector("list")
    for (trace.id in names.x)
      keys[[trace.id]] <-
        draw.key(list(title=factor.expressions[trace.id],
                      cex.title=key.cex.title,
                      border=TRUE,
                      text=list(
                        text=factor.levels[[trace.id]],
                        cex=key.cex.text),
                      lines=Rows(
                        trellis.par.get("superpose.line"),
                        seq(length(factor.levels[[trace.id]])))),
                 draw=FALSE)

    xyplot.list$legend <- list(left =
                               list(fun = legendGrob2wt,
                                    args = keys))
    xyplot.list$axis <- axis.i2wt
    ## recover()
  },
       s={})

  do.call("xyplot", xyplot.list)
}

"strip.interaction2wt" <-
function(which.given,
                                 which.panel,
                                 var.name,
                                 factor.levels,
                                 shingle.intervals,
                                 strip.names=c(TRUE,TRUE),
                                 style=1,
                                 ...) {
  strip.default(which.given=which.given,
                which.panel=which.panel,
                var.name=var.name,
                factor.levels=factor.levels,
                shingle.intervals=shingle.intervals,
                strip.names=strip.names,
                style=style,
                ...)
}


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
         simple.pch,
         data.x,
         col.by.row=TRUE,
         key.in=NULL ## list of key arguments for S-Plus
         ) {
  if (simple) {
## browser()
    pch.name <- character(0)
    if(!missing(simple.pch)) {
      pch.name <- names(simple.pch)[match(names(data.x), names(simple.pch), 0)]
      if (length(pch.name) > 1) pch.name <- pch.name[1]
    }
    if (length(pch.name) == 0)
    {
      pch.name <- names(data.x)[[1]]
      simple.pch <- list(seq(along=factor.levels[[pch.name]]))
      names(simple.pch) <- pch.name
    }
    pch <- list(NULL, NULL)
    names(pch) <- names(data.x)
    other.name <- names(factor.levels)[names(factor.levels) != pch.name]
    other.length <- length(factor.levels[[other.name]])
    pch[[other.name]] <- rep(simple.pch[[pch.name]], other.length)
    pch[[pch.name]] <- rep(simple.pch[[pch.name]], each=other.length)
  }
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
    
    row.panel <- row(tcL)[tcL==cell]
    column.panel <- col(tcL)[tcL==cell]

    these.labels <- c(trace.factor=names(factor.levels)[row.panel],
                      x.factor=names(factor.levels)[column.panel])
  }
       )
  
  trace.name <- these.labels["trace.factor"]
  trace.position <- factor.position[[trace.name]]
  trace.levels <- factor.levels[[trace.name]]
  x.name <- these.labels["x.factor"]
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
      names(ioh.list) <- c(x.name, other.name)
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
                                        pch=pch[[x.name]],
                                        box.ratio=box.ratio,
                                        ...)
           ,s=                          
           panel.bwplot.intermediate.hh(as.numeric.positioned(x.simple), y,
                                        horizontal=FALSE,
                                        col=tpg.col.simple[col.subscripts],
                                        pch=pch[[x.name]],
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
           panel.bwplot.intermediate.hh(as.numeric.positioned(x.factor), y,
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
      names(ioh.list) <- c(x.name, trace.name)
      position(ioh.list[[x.name]]) <- x.position
      position(ioh.list[[trace.name]]) <- trace.position
      if (!missing(simple.offset))
        ioh.list$b.offset <- simple.offset[[trace.name]]
      if (!missing(simple.scale))
        ioh.list$b.scale=simple.scale[[trace.name]]
      x.simple <- do.call("interaction.positioned", ioh.list)
##browser()
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
          suff.data.se <- eval(se, suff.data)
        }
        panel.intxplot(y=suff.data$y,
                       x=x.simple,
                       subscripts=seq(length(suff.data$y)),
                       groups=suff.data$tt,
                       offset.use=FALSE,
                       offset=position(x.simple), ##simple.offset[[trace.name]],
                       rug.use=TRUE,
                       se=suff.data.se,
                       ...)
      }
    }
    else {
      tab <- tapply(y, list(x, trace.values[subscripts]), fun)
      if.R(r={}, s=panel.lines <- lines)
      for (j in 1:ncol(tab))
        panel.lines(x=x.position, y=tab[,j], col=tpg.col[j], lty=tpg.lty[j], lwd=tpg.lwd[j])
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

    key.list <- list(title=trace.name,  ## S-Plus only
                     cex.title=1,
                     corner=c(0,.5), border=TRUE,
                     text=list(text=trace.levels, cex=.8),
                     lines=list(col=tpg.col, lty=tpg.lty, lwd=tpg.lwd))
    key.list[names(key.in)] <- key.in
    if.R(r={},
         s={
           if (is.null(key.list$x))
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
  
  ## if (side != "right")
  axis.default(side = side, scales = scales, ...)

  if (side == "bottom")
    {
      if (row.panel == 1)
        {

          axis.options <- axis.options$bottom
          at2     <- axis.options$at2                     
          labels2 <- axis.options$labels2                 
          rot2    <- axis.options$rot2
          cex2    <- axis.options$cex2
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

          axis.text2 <- trellis.par.get()$axis.text
          axis.text2$cex <- cex2
          
          labels2.units <-
            if (!is.null(at2) && !is.null(labels2)) 
              unit(1, "grobheight",
                   data = list(textGrob(unlist(labels2),
                     rot = rot2,
                     x=seq(along=unlist(labels2)),
                     just = if (rot2==90)
                     c("right", "center")
                     else c("center", "bottom"),
                     gp=do.call("gpar", axis.text2))))
            else
              unit(0, "lines")
          labels2.units <- convertHeight(labels2.units, "mm")
          
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
                      just = if (rot2==90) c("right", "center") else
                      c("center", "top"),
                      gp=do.call("gpar", axis.text2))
          
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
                      labels2.units - unit(8, "mm"),
                      just = c("center", "center"),
                      gp=do.call("gpar", trellis.par.get()$par.xlab.text))
          if (is.null(at3) && !is.null(labels3))
            grid.text(labels3[column.panel], rot=rot3,
                      y = unit(0, "npc") - tick.units - label.units -
                      labels2.units - unit(6, "mm"),
                      just = c("center", "center"),
                      gp=do.call("gpar", trellis.par.get()$par.xlab.text))
        }
    }
  
  ## if (side == "right")
  ##   {
  ##     if (column.panel == cols.per.page)
  ##       {
  ##         axis.options <- axis.options$right
  ##         at2     <- axis.options$at2                     
  ##         labels2 <- axis.options$labels2                 
  ##         rot2    <- axis.options$rot2
  ##         ticks2  <- axis.options$ticks2
  ##         at3     <- axis.options$at3                     
  ##         labels3 <- axis.options$labels3                 
  ##         rot3    <- axis.options$rot3
  ##         ticks3  <- axis.options$ticks3
  ##         if (is.null(rot2)) rot2 <- 0
  ##         if (is.null(rot3)) rot3 <- 0
  ##         if (is.null(ticks2)) ticks2 <- !is.null(labels2)
  ##         if (is.null(ticks3)) ticks3 <- FALSE

  ##         axis.units <- lattice.getOption("axis.units")$outer$right
  
  ##         ## space occupied by standard set of ticks
  ##         tick.units <-
  ##           ## if (is.null(scales$at)) unit(0, "lines")
  ##           ## else
  ##             with(axis.units$tick, unit(scales$tck * x, units))
          
  ##         ## space occupied by standard set of labels, assume 1.2
  ##         ## lines, but we have enough information to compute
  ##         ## explicitly (see how 'lab.grob' is computed in layout.R)
          
  ##         label.units <-
  ##           if (is.null(scales$at) || !scales$at) unit(0, "lines")
  ##           else unit(1.2, "lines")
          
  ##         ## want second set of ticks to extend beyond this.
          
  ##         if (!is.null(at2))
  ##           at2i <-
  ##             if (is.list(at2))
  ##               at2[[row.panel]]
  ##             else at2
          
  ##         if (!is.null(labels2))
  ##           labels2i <-
  ##             if (is.list(labels2))
  ##               labels2[[row.panel]]
  ##             else labels2

  ##         labels2.units <-
  ##           if (## !is.null(at2) &&  ## Not the same as bottom
  ##               !is.null(labels2))
  ##             unit(1, "grobwidth",
  ##                  data = list(textGrob(unlist(labels2)[1],
  ##                    rot = rot2,
  ##                    y=seq(along=unlist(labels2)[1]),
  ##                    just = if (rot2==90)
  ##                    c("center", "center")
  ##                    else c("left", "center"),
  ##                    gp=do.call("gpar", trellis.par.get()$axis.text))))
  ##           else
  ##             unit(0, "lines")
  ##         labels2.units <- convertWidth(labels2.units, "mm")

  ##         ## recover()          
  ##         right.margin <- unit(current.panel.limits()$xlim[2], "native")
  ##         if (ticks2 && (length(at2) > 0) && !is.na(at2))  ## not the same
  ##           grid.segments(y0 = unit(at2i, "native"),
  ##                         y1 = unit(at2i, "native"),
  ##                         x0 = right.margin,
  ##                         x1 = right.margin + tick.units +
  ##                         label.units)
          
  ##         if ((length(at2) > 0) && !is.na(at2) && length(labels2) > 0)  ## not the same
  ##           grid.text(labels2i, rot = rot2,
  ##                     y = unit(at2i, "native"),
  ##                     x = right.margin + tick.units + label.units +
  ##                     unit(1, "mm"),
  ##                     just = if (rot2==90) c("center", "center") else
  ##                     c("left", "center"),
  ##                     gp=do.call("gpar", trellis.par.get()$axis.text))
          
  ##         ## other set of labels (without ticks)
          
  ##         if (ticks3 && !is.null(at3))
  ##           grid.segments(y0 = unit(at3, "native"),
  ##                         y1 = unit(at3, "native"),
  ##                         x0 = right.margin,
  ##                         x1 = right.margin + tick.units +
  ##                         label.units + unit(1, "mm"))

  ##         if (!is.null(at3) && !is.null(labels3))
  ##           grid.text(labels3, rot=rot3,
  ##                     y = unit(at3, "native"),
  ##                     x = right.margin + tick.units +
  ##                     label.units +
  ##                     labels2.units +
  ##                     unit(5, "mm"),
  ##                     just = c("left", "top"),
  ##                     gp=do.call("gpar", trellis.par.get()$par.ylab.text))

  ##         if (is.null(at3) && !is.null(labels3))
  ##           grid.text(labels3[row.panel], rot=rot3,
  ##                     x = right.margin + tick.units +
  ##                     label.units +
  ##                     labels2.units +
  ##                     unit(5, "mm"),
  ##                     just = c("left", "top"),
  ##                     gp=do.call("gpar", trellis.par.get()$par.ylab.text))
  ##       }
  ##   }
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
  for (i in seq(length=nkeys))
    {
      key.gf <- placeGrob(key.gf, keys[[i]], row = i, col = 1)
    }
  key.gf
}

## source("c:/HOME/rmh/HH-R.package/HH/R/interaction2wt-new.R")
