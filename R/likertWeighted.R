likertWeighted <- function(x, ...) UseMethod("likertWeighted")

likertWeighted.formula <- function(x, data,
                     xlim=c(-100, 100),
                     scales=list(y=list(relation="free", cex=1.3),
                                 x=list(at=seq(-100, 100, 50), labels=abs(seq(-100, 100, 50)), cex=.5)),
                     box.ratio = 1000,
                     as.percent=TRUE,
                     rightAxis=FALSE,
                     between=list(x=1, y=0),
                     strip=FALSE, strip.left=FALSE,
                     par.settings=list(clip=list(panel="off")),
                     h.resizePanels=1,
                     auto.key.title=NULL,
                     auto.key.columns=dim(data)[[2]] - NumberOfConditioningVariables(formula), ## excludes conditioning variables
                     auto.key.cex=1.2,
                     auto.key.cex.title=1.2,
                     auto.key.lines.title=3,
                     ylab=NULL,
                     axis.top=dimnames(result)[[1]], ## Questions
                     axis.top.row=1,
                     ...) {

  NumberOfConditioningVariables <- function(formula) {
    NumCond <- length(strsplit(deparse(formula[[2]][[3]]), "+", fixed=TRUE)[[1]])
    if (NumCond==1 || NumCond==2) return(NumCond)
    stop("formula must have exactly one or two conditioning variables", .call=FALSE)
  }
  formula <- x
  force(auto.key.columns)

  result <- likert(formula, data,
                   ## col=col, ReferenceZero=ReferenceZero,
                   xlim=xlim,
                   scales=scales,
                   box.ratio=box.ratio,
                   as.percent=as.percent,
                   rightAxis=rightAxis,
                   between=between,
                   strip=strip, strip.left=strip.left,
                   par.settings=par.settings,
                   h.resizePanels=h.resizePanels,
                   ylab=ylab,
                   auto.key=list(title=auto.key.title, columns=auto.key.columns,
                                 cex=auto.key.cex, cex.title=auto.key.cex.title,
                                 lines.title=auto.key.lines.title),
                   ...)

  switch(length(dim(result)),
         result$layout <- c(1, dim(result)),
         {
           ## fix up row names for only first column
           dimresult <- dim(result)
           for (i in c(array(1:prod(dimresult), dimresult)[-1,]))
             result$y.limits[[i]] <- ""
           for (i in c(array(1:prod(dimresult), dimresult)[1,]))
             result$y.limits[[i]] <- rev(rep(dimnames(result)[[2]], times=dim(result)[1]))
         })

  result <- EmphasizeVerticalPanels(result, y.between=between$y)
  if (!is.null(axis.top))
    result <- result +
      latticeExtra::layer(if (current.row() %in% axis.top.row)
                            panel.axis("top", at=0, labels=axis.top[current.column()], outside=TRUE, rot=0),
                          data=list(axis.top=axis.top, axis.top.row=axis.top.row))
  result
}



EmphasizeVerticalPanels <- function(x, y.between) {
  x$y.between <- rep(y.between, dim(x)[length(dim(x))]) ## protect against scalar
  x$par.settings$axis.line$col <- "transparent" ## turn off automatic lines
  x$x.scales$col.line <- "black" ## turn back on x tick marks
  x +
    latticeExtra::layer({
      panel.abline(v=current.panel.limits()$xlim, col="black") ## turn on all vertical lines
      rc <- trellis.currentLayout()
      nrow <- dim(rc)[1]
      irow <- current.row() ## turn on select horizontal lines
      if (irow==1)
        panel.abline(h=current.panel.limits()$ylim[2], col="black", lwd=1)
      if ((irow < nrow) && y.between[irow] > 0)
        panel.abline(h=current.panel.limits()$ylim[1], col="black", lwd=1)
      if ((irow > 1) && (y.between[irow-1] > 0))
        panel.abline(h=current.panel.limits()$ylim[2], col="black", lwd=1)
      if (irow==nrow)
        panel.abline(h=current.panel.limits()$ylim[1], col="black", lwd=1)
    },
    data=list(y.between=x$y.between))
}


toCQxR <- function(x, C=1, R=2, Q=3) {
  newer <- aperm(x, c(C,Q,R))
  dim(newer) <- c(prod(dim(x)[c(C,Q)]), dim(x)[R])
  colnames(newer) <- dimnames(x)[[R]]
  result <- data.frame(newer,
                       group=rep(dimnames(x)[[Q]], each=dim(x)[C]),
                       row=rep(dimnames(x)[[C]], times=dim(x)[Q]),
                       check.names=FALSE)
  result
}


## matrix, data.frame, table(two-way)
likertWeighted.default <- function(x, ...) {
  if (inherits(x, "table")) x <- unclass(x)
  if (length(dim(x)) != 2) stop("likertWeighted requires a 2D or 3D array.", call.=FALSE)
  data <- data.frame(x, row=rownames(x), check.names=FALSE)
  formula <- ~ . | row
  likertWeighted(formula, data, ...)
}

## 3-way array
likertWeighted.array <- function(x, ..., C=1, Q=3, R=2) {
  if (length(dim(x)) == 2) return(NextMethod("likertWeighted"))
  if (length(dim(x)) != 3) stop("likertWeighted requires a 2D or 3D array.", call.=FALSE)
  data <- toCQxR(x, C=C, Q=Q, R=R)
  formula <-  ~ . | group + row
  likertWeighted(formula, data, ...)
}

