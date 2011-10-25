plot.likert <- function(x, ...)
  UseMethod("plot.likert")

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
                                middle.col="gray90",  ## "#F7F7F7" is the RColorBrewer default for middle.col in the RdBu scheme
                                col.strip.background="gray97",
                                col=mybrewer(x, middle.col),
                                as.percent=FALSE,
                                ...,
                                xName=deparse(substitute(x)),
                                rightAxisLabels=rowSums(abs(x)),
                                rightAxis=!missing(rightAxisLabels),
                                ylab.right=if (rightAxis) "Row Totals" else NULL,
                                panel=panel.likert) {
  force(xName)
  ## rightAxisMissing <- missing(rightAxis)  ## needed by as.percent
  x.input <- x
  x <- as.likert(x)
  force(rightAxis)
  force(rightAxisLabels)
  force(ylab.right)
  ## BrewerPaletteName <- match.arg(BrewerPaletteName)
  if (as.percent != FALSE) {
    x.pct <- x.input / rowSums(abs(x.input)) * 100
    x <- as.likert(x.pct)
    if (as.percent != "noRightAxis") {
      rightAxis <- TRUE
      if (is.null(ylab.right))
        ylab.right <- "Row Count Totals"
    }
    else
      rightAxis <- FALSE
  }
##recover()
  mybrewer <- function(x, middle.col) {

    is.odd <- function(x)  x%%2 == 1

    n <- attr(x,"nlevels")
    result <-
      if (n <= 2) {
        bp <- brewer.pal(n=3, name=BrewerPaletteName)
        if (n==1) bp[2] else bp[-2]
      }
      else {
        if (n <= 11)
          brewer.pal(n=n, name=BrewerPaletteName)
        else {
          if(attr(x, "even.col"))
            colorRampPalette(brewer.pal(n=10, name=BrewerPaletteName))(n)
          else
            colorRampPalette(brewer.pal(n=11, name=BrewerPaletteName))(n)
        }
      }
    lr <- length(result)
    if (is.odd(lr) && !missing(middle.col)) {
      middle <- (lr %/% 2) + 1
      result[middle] <- middle.col
    }
    result
  }

  auto.key.likert <- list(title=names(dimnames(x)[2]),
                          text=attr(x, "levels"),
                          cex=.7,
                          border=FALSE,
                          height=1,
                          space="bottom",
                          columns=min(2, length(attr(x, "levels"))), ## attr(x, "nlevels"),
                          padding.text=1,
                          size=5,
                          between=2,
                          between.columns=3,
                          just=.5,
                          reverse=FALSE,
                          rect=list(col=col, border=col),
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
  if (missing(middle.col)) middle.col ## "#F7F7F7"
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
                        par.settings=list(
                          strip.background=list(col=col.strip.background),
                          reference.line=list(col=reference.line.col),
                          layout.heights=list(xlab.key.padding=2),
                          clip=list(panel="off")),
                        reference.line=TRUE,
                        main=main,
                        rightAxisLabels=rightAxisLabels,
                        rightAxis=rightAxis,
                        panel=panel)
  barchart.args[names(dotdotdot)] <- dotdotdot
  if (!is.null(barchart.args$horizontal) && !barchart.args$horizontal) {
    tmp <- barchart.args$xlab
    barchart.args$xlab <- barchart.args$ylab
    barchart.args$ylab <- tmp
  }
  if (barchart.args$rightAxis) {
    barchart.args$par.settings$layout.widths$axis.key.padding <- 6
    barchart.args$ylab.right <- ylab.right
    ## ylab.right is not handled automatically because it is set in
    ## plot.likert.default and is not included in list(...)
  }
  do.call("barchart", barchart.args)
}

plot.likert.array <- function(x,  ## an array
                              condlevelsName=paste(names(dimnames(x))[-(1:2)], collapse="."),
                              xName=deparse(substitute(x)),
                              main=paste("layers of", xName, "by", condlevelsName),
                              layout=c(1, length(dim(x))-2),
                              positive.order=FALSE,
                              strip.left=TRUE,
                              strip.left.values=rev(names(tt.pl)), ## constructed from dimnames(x)[-(1:2)]
                              strip.left.par=list(cex=1, lines=1),
                              ...) {
  force(xName)
  if (length(dim(x))==2) NextMethod("plot.likert")
  tt <- as.MatrixList.array(x)  ## list of matrices, one per each layer of array
  tt.pl <- lapply(tt, plot.likert, positive.order=positive.order, ...) ## named list of likert plots
  tt.pl.nonames <- tt.pl ## if (strip.left) about to become unnamed list of likert plots
  if (strip.left) {
    names(tt.pl.nonames) <- NULL ## names are removed
    ResizeEtc(do.call("c", rev(tt.pl.nonames)),
              condlevelsName=condlevelsName,
              x.same=TRUE,
              layout=layout,
              strip.left.values=strip.left.values,
              strip.left.par=strip.left.par,
              resize.height=rep(dim(x)[1], layout[2])+1,
              main=main)
  } else {
    ResizeEtc(do.call("c", rev(tt.pl.nonames)),
            condlevelsName=condlevelsName,
              x.same=TRUE,
              layout=layout,
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
                             strip.left=TRUE,
                             strip.left.values=rev(names(x)),
                             strip.left.par=list(cex=1, lines=1),
                             main.middle=.5,
                             ...,
                             rightAxisLabels=sapply(x, rowSums, simplify=FALSE),
                             rightAxis=!missing(rightAxisLabels),
                             resize.height.tuning=-.5) {
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
                  MoreArgs=list(positive.order=positive.order, ...,
                                rightAxis=rightAxis),
                  SIMPLIFY=FALSE, USE.NAMES=TRUE)  ## named list of likert plots
  x.pl.nonames <- x.pl ## if (strip.left) about to become unnamed list of likert plots
  if (strip.left) {
    names(x.pl.nonames) <- NULL ## names are removed
    ResizeEtc(do.call("c", rev(x.pl.nonames)),
              condlevelsName=condlevelsName,
              x.same=TRUE,
              layout=layout,
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
              resize.height=rev(nRows+resize.height.tuning),
              main=main,
              main.middle=main.middle)
  }
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

## source("c:/HOME/rmh/HH-R.package/HH/R/likert.R")
