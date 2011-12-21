plot.likert <- function(x, ...)
  UseMethod("plot.likert")


## yscale.components.right.HH.good <- function(...) {
##   ans <- yscale.components.default(...)
##   ans$right <- ans$left
##   ans$right$labels$labels <- names(ans$left$labels$labels) ## this requires named left labels!
##   ans
## }
## ## environment(yscale.components.right.HH) <- environment(plot.likert)

yscale.components.right.HH <- function(...) {
  ans <- yscale.components.default(...)
  ans$right <- ans$left
  ans$right$labels$labels <- names(ans$right$labels$labels) ## this requires named left labels!
  ans
}
## environment(yscale.components.right.HH) <- environment(plot.likert)

## yscale.components.right.HH.list.of.panels.NorightAxis <- function(...) {
##   ans <- yscale.components.default(...)
##   lr <- list(...)$right
##   if (!is.null(lr) && lr) {
##     ans$right <- ans$left
##     ans$right$labels$labels <- get("x", env=sys.parent())$y.limits.right[list(...)[[1]]]
##   }
##   ans
## }
## ## environment(yscale.components.right.HH) <- environment(plot.likert)

## yscale.components.right.HH.list.of.panels <- function(...) { ## currently unused
##   ans <- yscale.components.default(...)
##   ans$right <- ans$left
##   scalls <- sys.calls()
##   my.calls <- sapply(scalls, `[[`, 1)
##   which.call <- match("print.trellis", my.calls)
##   ylr <- scalls[[which.call]][[2]]$y.limits.right
##   yl <- scalls[[which.call]][[2]]$y.limits
##   ##recover()
##   ans$right$labels$labels <- names(ans$right$labels$labels)
##   ans
## }
## ## environment(yscale.components.right.HH) <- environment(plot.likert)

panel.likert <- function(..., rightAxisLabels, rightAxis) {
  panel.barchart(...)
  if (rightAxis) panel.axis.right(side="right", at=1:length(rightAxisLabels),
                                  labels=rightAxisLabels, outside=TRUE)
}

plot.likert.default <- function(x,
                                positive.order=FALSE,
                                ylab=names(dimnames(x)[1]),
                                xlab="Frequency",
                                main=xName,
                                BrewerPaletteName="RdBu",
                                ## These are the diverging palettes in RColorBrewer
                                ## c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr",
                                ## "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                reference.line.col="gray65",
                                middle.color="gray90",  ## "#F7F7F7" is the RColorBrewer default for middle.color in the RdBu scheme
                                col.strip.background="gray97",
                                col=brewer.pal.likert(attr(x, "nlevels"), BrewerPaletteName, middle.color),
                                as.percent=FALSE,
                                ...,
                                key.border.white=TRUE,
                                xName=deparse(substitute(x)),
                                rightAxisLabels=rowSums(abs(x)),
                                rightAxis=!missing(rightAxisLabels),
                                ylab.right=if (rightAxis) "Row Totals" else NULL,
                                panel=panel.barchart,
                                yscale.components=yscale.components.right.HH) {
  force(xName)
  ## rightAxisMissing <- missing(rightAxis)  ## needed by as.percent
  x.input <- x
  x <- as.likert(x, reverse=TRUE)
  force(rightAxis)
  force(rightAxisLabels)
  force(ylab.right)
  ## BrewerPaletteName <- match.arg(BrewerPaletteName)
  if (as.percent != FALSE) {
    x.pct <- x.input / rowSums(abs(x.input)) * 100
    x <- as.likert(x.pct, reverse=TRUE)
    if (as.percent != "noRightAxis") {
      rightAxis <- TRUE
      if (is.null(ylab.right))
        ylab.right <- "Row Count Totals"
    }
    else
      rightAxis <- FALSE
  }
##recover()

  auto.key.likert <- list(title=names(dimnames(x)[2]),
                          text=attr(x, "levels"),
                          cex=.7,
                          border=FALSE,
                          height=1,
                          space="bottom",
                          columns=attr(x, "nlevels"),
##                          columns=min(2, length(attr(x, "levels"))), ## attr(x, "nlevels"),
                          padding.text=1,
                          size=2,
                          between=.5,
                          between.columns=2,
                          just=.5,
                          reverse=FALSE,
                          rect=list(col=col, border=if (key.border.white) "white" else col),
                          ##                ## The next two lines suppress unwanted automatic displays.
                          points=FALSE,     ## This line is necessary when the right axis is used.
                          rectangles=FALSE) ## This line is necessary and not redundant.

  dotdotdot <- list(...)
  if (match("auto.key", names(dotdotdot), 0) != 0) {
    ak <- dotdotdot$auto.key
    auto.key.likert[names(ak)] <- ak
    dotdotdot$auto.key <- NULL
  }
  ## auto.key.likert$rect=list(
  ##   col=col, border=col,
  ##   height=auto.key.likert$height, size=auto.key.likert$size)

  if (missing(ylab) && (is.null(ylab)||is.na(ylab))) ylab <- NULL
  ## RColorBrewer diverging palettes: c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  ## These are the middle colors from RCOlorBrewer:
  ## > for (i in c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
  ## + print(c(i, brewer.pal(n=3, name=i)[2]))
  ## [1] "RdBu"     "#F7F7F7"
  ## [1] "BrBG"     "#F5F5F5"
  ## [1] "PiYG"     "#F7F7F7"
  ## [1] "PRGn"     "#F7F7F7"
  ## [1] "PuOr"     "#F7F7F7"
  ## [1] "RdGy"     "#FFFFFF"
  ## [1] "RdYlBu"   "#FFFFBF"
  ## [1] "RdYlGn"   "#FFFFBF"
  ## [1] "Spectral" "#FFFFBF"

  nc <- ncol(x)
  if (missing(middle.color)) middle.color ## "#F7F7F7"
  ## if middle.color is missing as an argument, then use the default value from the argument list
  if (attr(x, "even.col")) {
    likert.palette=col[c((nc/2):1, ((nc/2)+1):nc)]
  }
  else {
    likert.palette=col[c((nc/2):1, ((nc/2)+1):(nc-1))]
  }
  if (positive.order) {
    x.attr <- attributes(x)
    x.attr$dim <- NULL
    x.attr$dimnames <- NULL
    x.attr$original.order <- order(x.attr$positive.order)
    x <- x[x.attr$positive.order,, drop=FALSE]
    attributes(x)[names(x.attr)] <- x.attr
    rightAxisLabels <- rightAxisLabels[x.attr$positive.order]
  }
  barchart.args <- list(x=x,
                        col=likert.palette,
                        border=likert.palette,
                        auto.key=auto.key.likert,
                        xlab=xlab, ylab=ylab,
                        ylab.right=ylab.right,
                        par.settings=list(
                          strip.background=list(col=col.strip.background),
                          reference.line=list(col=reference.line.col),
                          layout.heights=list(
                            main.key.padding=2.5,
                            key.axis.padding=0,
                            axis.top=.75,
                            xlab.key.padding=2),
                          clip=list(panel="off")),
                        reference.line=TRUE,
                        main=main,
                        rightAxisLabels=rightAxisLabels,
                        rightAxis=rightAxis,
                        panel=panel,
                        yscale.components=yscale.components)
  barchart.args[names(dotdotdot)] <- dotdotdot
  if (!is.null(barchart.args$horizontal) && !barchart.args$horizontal) {
    tmp <- barchart.args$xlab
    barchart.args$xlab <- barchart.args$ylab
    barchart.args$ylab <- tmp
  }
  ## if (barchart.args$rightAxis) {
  ##   barchart.args$par.settings$layout.widths$axis.key.padding <- 6
  ##   ## this is a glitch until I understand layout.widths$right.axis
  ## }
  result <-do.call("barchart", barchart.args)
  if (rightAxis) {
    result$y.scales$alternating <- 3
##    result$y.limits.right <- rightAxisLabels
    names(result$y.limits) <- rightAxisLabels
    class(result) <- c("trellis.right.HH", class(result))
  }
  result
}
## environment(plot.likert.default) <- environment(plot.likert)

plot.likert.array <- function(x,  ## an array
                              condlevelsName=paste(names(dimnames(x))[-(1:2)], collapse="."),
                              xName=deparse(substitute(x)),
                              main=paste("layers of", xName, "by", condlevelsName),
                              layout=c(1, length(dim(x))-2),
                              positive.order=FALSE,
                              strip=TRUE,
                              strip.left=TRUE,
                              strip.left.values=rev(names(tt.pl)), ## constructed from dimnames(x)[-(1:2)]
                              strip.values=rev(names(tt.pl)),
                              strip.left.par=list(cex=1, lines=1),
                              ...) {
  force(xName)
  if (length(dim(x))==2) NextMethod("plot.likert")
  tt <- as.MatrixList.array(x)  ## list of matrices, one per each layer of array
  tt.pl <- lapply(tt, plot.likert, positive.order=positive.order, ...) ## named list of likert plots
  tt.pl.nonames <- tt.pl ## if (strip.left) about to become unnamed list of likert plots
  names(tt.pl.nonames) <- NULL ## names are removed
  if (strip.left) {
    ResizeEtc(do.call("c", rev(tt.pl.nonames)),
              condlevelsName=condlevelsName,
              x.same=TRUE,
              layout=layout,
              strip=strip,
              strip.left.values=strip.left.values,
              strip.left.par=strip.left.par,
              resize.height=rep(dim(x)[1], layout[2])+1,
              main=main)
  } else {
    ResizeEtc(do.call("c", rev(tt.pl.nonames)),
            condlevelsName=condlevelsName,
              x.same=TRUE,
              layout=layout,
              strip=strip,
              strip.values=strip.values,
              resize.height=rep(dim(x)[1], layout[2])+1,
              main=main)
  }
}

plot.likert.list <- function(x,  ## named list of matrices, 2D tables, 2D ftables, or 2D structables, or all-numeric data.frames
                             condlevelsName="ListNames",
                             xName=deparse(substitute(x)),
                             main=paste("List items of", xName, "by", condlevelsName),
                             layout=c(1, length(x)),
                             positive.order=FALSE,
                             strip=TRUE,
                             strip.left=TRUE,
                             strip.left.values=rev(names(x)),
                             strip.values=rev(names(x)),
                             strip.left.par=list(cex=1, lines=1),
                             main.middle=.5,
                             ...,
                             rightAxisLabels=sapply(x, rowSums, simplify=FALSE),
                             rightAxis=!missing(rightAxisLabels),
                             resize.height.tuning=-.5,
                             yscale.components=yscale.components.right.HH) {
  force(xName)
  for (nxi in names(x)) { ## convert vectors to single-row matrices
    xi <- x[[nxi]]
    if (is.numeric(xi) && is.null(dim(xi))) x[[nxi]] <- t(xi)
  }
  if (!is.listOfNamedMatrices(x)) {
    if (is.null(names(x))) stop("Items in a list for plot.likert must be named.")
    if (!all(sapply(x, function(x) length(dim(x))) == 2))
      stop("All items in a list for plot.likert must have at most two dimensions.")
    if (!all(sapply(x, ncol) == ncol(x[[1]])))
      stop("All items in a list for plot.likert must have the same number of columns.")
    if (is.data.frame(x))
      stop("plot.likert.list does not accept a data.frame.\nPlease use plot.likert.data.frame.")
    if (sapply(x, function(xx) is.data.frame(xx) && !all(sapply(xx, is.numeric))))
      stop("A data.frame in a plot.likert.list argument must have only numeric columns.")
  }
  nRows <- sapply(x, nrow)
  x.pl <- mapply(plot.likert, x,
                 rightAxisLabels=sapply(rightAxisLabels, rev, simplify=FALSE),
                 MoreArgs=list(
                   positive.order=positive.order, ...,
                   rightAxis=rightAxis,
                   yscale.components=yscale.components),
                  SIMPLIFY=FALSE, USE.NAMES=TRUE)  ## named list of likert plots
  x.pl.nonames <- x.pl ## if (strip.left) about to become unnamed list of likert plots
  names(x.pl.nonames) <- NULL ## names are removed
  result <-
    if (strip.left) {
      ResizeEtc(do.call("c", rev(x.pl.nonames)),
                condlevelsName=condlevelsName,
                x.same=TRUE,
                layout=layout,
                strip=strip,
                strip.left.values=strip.left.values,
                strip.left.par=strip.left.par,
                resize.height=rev(nRows+resize.height.tuning),
                main=main,
                main.middle=main.middle)
    } else {
      ResizeEtc(do.call("c", rev(x.pl.nonames)),
                condlevelsName=condlevelsName,
                x.same=TRUE,
                layout=layout,
                strip=strip,
                strip.values=strip.values,
                resize.height=rev(nRows+resize.height.tuning),
                main=main,
                main.middle=main.middle)
    }
  result$y.limits.right <- rightAxisLabels
  result
}

plot.likert.table <- function(x, ..., xName=deparse(substitute(x))){
  force(xName)
  class(x) <- "array"
  plot.likert(x, xName=xName, ...)
}
plot.likert.ftable <- function(x, ..., xName=deparse(substitute(x))){
  force(xName)
  plot.likert(as.table(x), xName=xName, ...)
}
plot.likert.structable <- function(x, ..., xName=deparse(substitute(x))){
  force(xName)
  plot.likert(as.table(x), xName=xName, ...)
}
## plot.likert.numeric <- function(x, ..., xName=deparse(substitute(x))){
##   force(xName)
##   plot.likert(as.likert(x, xName=xName), xName=xName, ...)
## }
plot.likert.data.frame <- function(x, ..., xName=deparse(substitute(x))){
  force(xName)
  plot.likert(data.matrix(x), xName=xName, ...)
}


brewer.pal.likert <- function(n, name,  middle.color) {
  
  is.odd <- function(x)  x%%2 == 1
  
  palette <-
    if (n <= 2) {
      bp <- brewer.pal(n=3, name=name)
      if (n==1) bp[2] else bp[-2]
    }
    else {
      if (n <= 11)
        brewer.pal(n=n, name=name)
      else {
        if (is.odd(n))
          colorRampPalette(brewer.pal(n=11, name=name))(n)
        else
          colorRampPalette(brewer.pal(n=10, name=name))(n)
      }
    }
  if (is.odd(n) && !missing(middle.color)) {
    middle <- (n %/% 2) + 1
    palette[middle] <- middle.color
  }
  palette
}


## source("c:/HOME/rmh/HH-R.package/HH/R/likert.R")
