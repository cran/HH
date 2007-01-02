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
    if.R(r={
      cpl <- current.panel.limits()
      pushViewport(viewport(xscale = cpl$xlim,
                            yscale = cpl$ylim,
                            clip = "off"))
      ## put anything you want unclipped inside this:
      panel.axis("bottom", outside=TRUE, at=x.position, labels=x.levels, rot=0)
      panel.text(x=mean(cpl$xlim), y=cpl$ylim[1]-.20*diff(cpl$ylim),
                 x.name, adj=.5)
      ## end of unclipped part
      upViewport()
    },
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
    if.R(r={
      cpl <- current.panel.limits()
      key.x.offset.r=cpl$xlim[1] - .6*diff(cpl$xlim[1:2])
      pushViewport(viewport(xscale = cpl$xlim,
                            x = unit(key.x.offset.r, "native"),
                            yscale = cpl$ylim,
                            clip = "off"))
      ## put anything you want unclipped inside this:
      draw.key(key.list, draw=TRUE)
      ## end of unclipped part
      upViewport()
    },
         s={
           key.list$x <- par()$usr[1]-.6*diff(par()$usr[1:2])
           do.call("key", key.list)
         }
         )
  }


  ## y label
  if (column.panel==cols.per.page) {
    if.R(r={
      cpl <- current.panel.limits()
      pushViewport(viewport(xscale = cpl$xlim,
                            yscale = cpl$ylim,
                            clip = "off"))
      ## put anything you want unclipped inside this:
      panel.text(x=cpl$xlim[2]+.2*diff(cpl$xlim), y=mean(cpl$ylim),
                 responselab, adj=0)
      ## end of unclipped part
      upViewport()
    },
         s={
           mtext(responselab, side=4, line=3, at=mean(par()$usr[3:4]))
         }
         )
  }

## 
## ## main diagonal
## bwplot or simple.bwplot
## ## argument simple=TRUE and exactly two factors
## 
## ## off-diagonal
## intx plot
## 
## ## bottom row
## x variable name
## 
## ## left side
## trace variable name and key
## 
## ## right side
## response variable name
## 
## ## top
## main title



## row
## column
## cols.per.page
## rows.per.page
## 
## ## main diagonal
## bwplot or simple.bwplot
## ## argument simple=TRUE and exactly two factors
## 
## ## off-diagonal
## intx plot
## 
## ## bottom row
## x variable name
## 
## ## left side
## trace variable name and key
## 
## ## right side
## response variable name
## 
## ## top
## main title
}
