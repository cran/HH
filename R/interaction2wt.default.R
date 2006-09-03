"interaction2wt.default" <-
function(x, response.var, responselab=deparse(substitute(y)),
           relation=list(x="free", y="same"),
           x.relation=relation$x, y.relation=relation$y,
           digits=3,
           x.between=c(1,rep(0,ncol(x)-1)), y.between=NULL,
           cex=.75,
           panel.input=panel.interaction2wt,
           strip.input=strip.interaction2wt,
           par.strip.text.input=list(cex=.7),
           scales.additional,
           main.in=paste(responselab,
             ": main effects and 2-way interactions", sep=""),
           xlab.in=TRUE,
           xlab=list(labels=""), ylab=list(labels=""),
           ...,
           main.cex) {
  n <- nrow(x)
  k <- ncol(x)
  names.x <- names(x)
  for (i in names.x) x[[i]] <- as.factor(x[[i]])
  x.list <- as.list(x)

  for (i in names(x.list)) {
    xi <- x.list[[i]]
    ## browser()
    if (!is.null(attr(xi,"position")))
      x.list[[i]] <- attr(xi,"position")[xi]
    else {
      if (inherits(xi, "ordered")) {
        old.warn <- options(warn=-1)
        if (!any(is.na(as.numeric(levels(xi)))))
          x.list[[i]] <- as.numeric(as.character((xi)))
        options(old.warn)
      }
      else if (inherits(xi, "factor")) {
          x.list[[i]] <- as.numeric((xi))
      }
    }
  }

##  scales.input <- list(x=list(relation=x.relation, alternating=FALSE, xaxt="n"),
##                       y=list(relation=y.relation, alternating=2))
  factor.levels <- lapply(x, attributes)
  scales.input=list(x=list(
                        relation=x.relation, alternating=1,
                        at=rep(c(key=-1, lapply(factor.levels,
                          function(x) {
                            if (!is.null(x$position))
                              x.levels <- x$position
                            else {
                              if (any(x$class=="ordered")) {
                                old.warn <- options(warn=-1)
                                if (!any(is.na(as.numeric(x$levels))))
                                  x.levels <- as.numeric(as.character((x$levels)))
                                else
                                  x.levels <- seq(along=x$levels)
                                options(old.warn)
                              }
                              else
                                x.levels <- seq(along=x$levels)
                            }
                            x.levels
                          }
                          )),
                               length(factor.levels)),
                        labels=rep(c(
                          key="",
                          lapply(factor.levels,
                                 function(x) as.character(x$levels))),
                               length(factor.levels))
                        ),
                      y=list(relation=y.relation, alternating=2))
  names(scales.input$x$at) <- NULL
  names(scales.input$x$labels) <- NULL
  if (!missing(scales.additional)) {
    scales.input$x[names(scales.additional$x)] <- scales.additional$x
    scales.input$y[names(scales.additional$y)] <- scales.additional$y
  }
  ccd <- data.frame(response.var=rep(response.var, length=n*k*k),
                    x.values    =unlist(rep(x.list, k)),
                    trace.values=unlist(rep(x.list, rep(k,k))),
                    x.factor    =factor(rep(rep(names.x, rep(n,k)), k),
                      levels=c("key",names.x)),
                    trace.factor=factor(rep(    names.x, rep(n*k,  k)),
                      levels=names.x))
  ccd <- rbind(ccd,
               data.frame(response.var=response.var[1],
                          x.values=1,
                          trace.values=1,
                          x.factor="key",
                          trace.factor=names.x))
  
  formula <- response.var ~ x.values | x.factor * trace.factor
  if (!missing(main.cex)) {
    main.in <- as.list(main.in)
    main.in$cex <- main.cex
  }
  xyplot(formula,
         data=ccd,
         responselab=responselab,
         trace.values=ccd$trace.values,
         factor.levels=factor.levels,
         between=list(x=x.between, y=y.between),
         scales=scales.input,
         xaxs="e",
         prepanel=function(x,y) list(xlim=range(x)+c(-1,1)),
         panel=panel.input,
         strip=strip.input,
         par.strip.text=par.strip.text.input,
         layout=c(k+1, k),
##       skip=c(TRUE, rep(FALSE, k)),
         main=main.in,
         xlab.in=xlab.in,
         xlab=list(labels=""), ylab=list(labels=""),
         cex=cex, las=1, ...)
}

