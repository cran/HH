interaction2wt <- function(x, ...)
  UseMethod("interaction2wt")

"interaction2wt.formula" <-
function(x, data=sys.parent(), responselab,
			    ...) {
  if.R(
       r={
         dft <- do.formula.trellis.xysplom(x, data=data)
         y.in <- dft$y[[1]]
         x.in <- dft$x
         responselab <- names(dft$y)
       },
       s={
         dft <- do.formula.trellis(x)
         y.in <- eval(dft$expr[[1]], local=data)
         x.in <- data[,dft$xlab,drop=FALSE]
         responselab <- dft$ylab
       })
  if (is.null(x.in) || is.null(y.in))
    stop("both x and y are needed in formula")
  interaction2wt.default(x=x.in, response.var=y.in,
                              responselab=responselab,
                              ...)
}

interaction2wt.default <-
  function(x, response.var, responselab=deparse(substitute(y)),
           relation=list(x="same", y="same"),
           x.relation=relation$x, y.relation=relation$y,
           digits=3,
           x.between=if (label.as.interaction.formula) 0 else 1,
           y.between=if (label.as.interaction.formula) 0 else 1,
           between,
           cex=.75,
           rot=c(0,0),
           panel.input=panel.interaction2wt,
           strip.input=if (label.as.interaction.formula) strip.default
                       else strip.interaction2wt,
           par.strip.text.input=list(cex=.7, responselab=responselab),
           scales.additional,
           main.in=paste(responselab,
             ": main effects and 2-way interactions", sep=""),
           xlab=list(labels=""), ylab=list(labels=""),
           simple=FALSE,
           box.ratio=if (simple) .32 else 1,
           label.as.interaction.formula=TRUE,
           ...,
           main.cex
           ) {
  n <- nrow(x)
  k <- ncol(x)
  names.x <- names(x)

  if (k<2) stop("interaction2wt requires at least two factors.")
  if (simple && k != 2) stop("Simple effects requires exactly two factors.")
  
  x.list <- x
  for (i in names.x) {
    x[[i]] <- as.factor(x[[i]])
    x.list[[i]] <- as.numeric(x[[i]])
  }
  
  factor.levels <- lapply(x, levels)
  factor.position <- lapply(x, position)

  scales.input <- list(x=list(
                         relation=x.relation,
                         alternating=FALSE,
                         xaxt="n",  ## S-Plus
                         draw=FALSE ## R
                         ),
                       y=list(relation=y.relation, alternating=2))

  if (!missing(scales.additional)) {
    scales.input$x[names(scales.additional$x)] <- scales.additional$x
    scales.input$y[names(scales.additional$y)] <- scales.additional$y
  }
  if.R(r={
    scales.input$x$at <- NULL
    scales.input$rot <- rep(rot,2)
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
         trace.values=ccd$trace.values,
         factor.levels=factor.levels,
         factor.position=factor.position,
         between=if (missing(between))
         list(x=x.between, y=y.between)
         else
         between,
         scales=scales.input,
         xaxs="e",
         prepanel=function(x,y) list(xlim=range(x)+c(-1,1)),
         panel=panel.input,
         strip=strip.input,
         par.strip.text=par.strip.text.input,
         layout=c(k, k),
         main=main.in,
         xlab=list(labels=""), ylab=list(labels=""),
         cex=cex, las=1, aspect=1,
         simple=simple,
         data.x=x,
         box.ratio=box.ratio,
         ...)
  if.R(r={
    xyplot.list$lattice.options <-
      list(axis.options=list(
             bottom=list(
               at2=factor.position,
               labels2=factor.levels,
               rot2=rot[1],
               labels3=levels(ccd$x.factor)),
             right=list(
               at2=pretty(current.panel.limits()$ylim),
               labels2=pretty(current.panel.limits()$ylim),
               rot2=rep(rot,2)[2],
               labels3=rep(responselab, k)
               )
             ),
           layout.heights=list(axis.xlab.padding=list(x=15, units="mm")),
           layout.widths=list(right.padding=list(x=13, units="mm")))
    keys <- vector("list")
    for (trace.id in names.x)
      keys[[trace.id]] <-
        draw.key(list(title=trace.id,
                      cex.title=1,
                      border=TRUE,
                      text=list(text=factor.levels[[trace.id]], cex=.8),
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

