## enforces unique names for panels
likertWeighted47 <- function(x, data,
                           xlim=c(-100,100), x.at=seq(-100,100,25), x.labels=abs(x.at),
                           h.resizePanels=data$rowheights,
                           rightAxisLabels=format(round(h.resizePanels, digits),
                                                  big.mark=","),
                           ylab=NULL,
                           axis.key.padding=10,
                           y.tck=c(0,3),
                           layout=c(1, nrow(data)),
                           as.percent=TRUE,
                           line.color="black",
                           box.ratio=1000,
                           digits=-3,
                           scales.cex=1,
                           between=list(y=0),
                           layer=TRUE,
                           ...) {
  n.panels <- nrow(data)
  conditioning.var.name <- x[[2]][[3]]
  conditioning.var.value <- data[[x[[2]][[3]]]]
  if (length(unique(conditioning.var.value)) != length(conditioning.var.value))
    stop("Entries in conditioning variable must be unique.", call.=FALSE)
  result <-
    likert(x, data,
           rightAxisLabels=rightAxisLabels,
           ylab=ylab,
           scales=list(x=list(limits=xlim, at=x.at, labels=x.labels),
                       y=list(relation="free", tck=y.tck),
                       cex=scales.cex),
           h.resizePanels=h.resizePanels,
           box.ratio=box.ratio,
           strip=FALSE, between=between,
           par.settings=list(
             axis.line=list(col="transparent"),
             layout.widths=list(ylab.right=1, axis.key.padding=axis.key.padding)
           ),
           as.percent=as.percent,
           layout=layout,
           ...)
  if (layer)
    result +
      latticeExtra::layer({
        between.vector <- rep(between$y, length=n.panels)
        bvn0 <- which(between.vector != 0)
        panel.abline(v=current.panel.limits()$xlim, col.line=line.color)
        if (panel.number()==1)
          panel.axis("top", labels=FALSE, line.col=line.color, outside=TRUE, at=x.at)
        if (panel.number() %in% c(1, bvn0+1))
          panel.abline(h=current.panel.limits()$ylim[2], col.line=line.color)
        if (panel.number()==n.panels)
          panel.axis("bottom", labels=FALSE, line.col=line.color, outside=TRUE, at=x.at)
        if (panel.number() %in% c(n.panels, bvn0))
          panel.abline(h=current.panel.limits()$ylim[1], col.line=line.color)
      },
      data=list(n.panels=n.panels, line.color=line.color, x.at=x.at, between=between))
  else
    result
}
