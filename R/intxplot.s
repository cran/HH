intxplot <- function(x, data=sys.parent[1], groups.in,
                     scales,
                     key.length=1,
                     key.lines,
                     key=TRUE,
                     trace.factor.name=deparse(substitute(groups.in)),
                     x.factor.name=x.factor,
                     xlab=x.factor.name,
                     main=list(main.title, cex=main.cex),
                     condition.name="condition",
                     panel="panel.intxplot",
                     summary.function="sufficient",
                     se,
                     ...,
                     data.is.summary=FALSE,
                     main.title=paste(
                       "Interactions of", trace.factor.name, "and",
                       x.factor.name,
                       if (length(x[[3]]) > 1)
                       paste("|", condition.name.to.use)),
                     main.cex=1.5) {
  ## on.exit(browser())
  M <- sys.call()
  M[[1]] <- as.name("xyplot")

  groups <- eval(substitute(groups.in), data)
  levels.groups <- levels(as.factor(groups))

  ## browser()

  if (length(x[[3]]) > 1) {
    x.factor <- deparse(x[[3]][[2]])
    M[[2]][[3]][[2]] <- parse(text=paste("as.numeric.positioned(", x.factor, ")"))[[1]]

    condition.name.to.use <-
      if ((class(x[[3]][[3]]) == "name") && missing(condition.name))
        deparse(x[[3]][[3]])
      else
        condition.name
    
    M$strip=parse(text=paste(
                    sep="",
                    "function(..., var.name)",
                    "strip.default(..., strip.names=c(TRUE,TRUE), var.name='",
                    condition.name.to.use,
                    "')"))[[1]]
  }
  else {
    x.factor <- deparse(x[[3]])
    M[[2]][[3]] <- parse(text=paste("as.numeric.positioned(", x.factor, ")"))[[1]]
    condition.name.to.use <- ""
  }

  xf <- data[[x.factor]]
  lev.x <- levels(xf)
  num.lev.x <- position(xf)

  if (data.is.summary)
    M$data <- data
  else
    if (is.null(summary.function)) {}
    else
      if (is.character(summary.function)) {
        switch(summary.function,
               sufficient=
               M$data <- sufficient(data,
                                    yname=deparse(x[[2]]),
                                    c(x.factor.name, trace.factor.name)),
               
               bwplot=stop("bwplot not yet implemented inside 'intxplot'."),
               stop(paste("summary function ",
                          summary.function,
                          " not known yet.", sep=""))
               )
      }
      else
        stop(paste("summary function ",
                   deparse(substitute(summary.function)),
                   " not known yet.", sep=""))
  ## browser()
  M$scales <- list(x=list(
                     at=num.lev.x,
                     labels=lev.x,
                     alternating=FALSE))
  if (!missing(scales)) {
    if (!is.null(scales$x))
      M$scales$x[names(scales$x)] <- scales$x
    if (!is.null(scales$y))
      M$scales$y[names(scales$y)] <- scales$y
    scales$x <- NULL
    scales$y <- NULL
    if (length(scales) > 0)
      M$scales[names(scales)] <- scales
  }
  if (missing(xlab))
    M$xlab <- x.factor.name    ## xlab is needed to avoid "as.numeric()"
  
  tpg <- trellis.par.get("superpose.line")
  if (key) {
    key.index <- rep(1:length(tpg$col), length=length(levels.groups))
    M$key <- list(
                  lines = Rows(tpg, key.index),
                  text = list(levels.groups),
                  columns = key.length,
                  title = trace.factor.name,
                  cex.title=1,
                  space="right",
                  border=1)
  }
  else
    M$key <- NULL
  if (missing(main))
    M$main <- list(main.title, cex=main.cex)
  if (missing(panel)) M$panel <- panel
  if (missing(key.lines) && !is.null(list(...)$par.settings$superpose.line))
    key.lines <- list(...)$par.settings$superpose.line
  if (!missing(key.lines)) M$key$lines[names(key.lines)] <- key.lines

  M$key.length <- NULL
  M$key.lines <- NULL
  M$condition.name <- NULL
  M$trace.factor.name <- NULL
  M$x.factor.name <- NULL
  M$main.title <- NULL
  M$main.cex <- NULL
  ## browser()

  if (!missing(se)) {
    if (!is.logical(substitute(se)))
      M$se <- eval(substitute(se), M$data)
    else
      M$se <- M$data$sd/sqrt(M$data$nobs)
  }

  eval(M, sys.parent(1))
}
## trace(intxplot, exit=browser)



## interaction lines and Confidence Intervals (if se is not missing)
## and offsets for each group level
## Add rug() for offsets
panel.intxplot <-
  function(x, y, subscripts, groups, type = "l", ..., se, cv=1.96,
           offset.use=(!missing(groups) && !missing(se)),
           offset.scale=2*max(as.numeric.positioned(groups)),
           offset=
           as.numeric.positioned(groups[match(levels(groups), groups)]) / offset.scale,
           rug.use=offset.use)
{
  ox <- order(x)
  x <- x[ox]
  y <- y[ox]
  subscripts <- subscripts[ox]

  x.adjust <-
    if (offset.use) {
      position(x) +
        (offset-mean(offset))[as.numeric(unpositioned(groups[subscripts]))]
    }
    else
      x

  col <- rep(trellis.par.get("superpose.line")$col,
             length=length(levels(groups)))
  g <- groups[subscripts]
  tpg.col <- rep(col, length=length(g))

  panel.superpose(as.position(x.adjust), y, subscripts, unpositioned(groups),
                  type = "l", col=col, ...)

  if (rug.use) {
    for (i in seq(along=levels(g))) {
      xag <- x.adjust[g==levels(g)[i]]
      if (length(xag)) panel.rug(as.position(xag),
                                 col=col[i],  ticksize = +0.03, lwd=1)
    }
  }
  
  if (!missing(se)) {
    if (is.logical(se)) warning("se is logical. Numerical value se=1 is used.")
    if (length(se)==1) se=rep(se, length(groups))
    if (length(cv)==1) cv=rep(cv, length(groups))
    se.cv <- se*cv
    
    for (i in seq(along=x)) {
      panel.superpose(x=rep(position(x.adjust)[i], 2),
                      y=y[i] + c(-1,1)*se.cv[subscripts[i]],
                      subscripts[c(i,i)], unpositioned(groups), type = "l",
                      col=tpg.col[i], ...)
      
##      browser()
    }
  }
}
## trace(panel.intxplot, exit=browser)
## trace(sufficient, exit=browser)
