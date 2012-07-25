as.TwoTrellisColumns <- function(left,  ## left  is the left trellis object
                                   right, ## right is the right trellis object
                                   ## Both left and right must have identical
                                   ## settings for number and size of vertical panels,
                                   ## left-axis labels, number of lines in main, sub, legend.
                                   ...,
                                   panel.width=.48,
                                   px=list(
                                     L=c(0, panel.width),
                                     R=c(1-panel.width, 1),
                                     M=c(panel.width, 1-panel.width)),
                                   keepLegend=TRUE) {
  
  middle <- left
  
  left$main  <- " "
  right$main <- " "
  if (!is.null( left$sub))  left$sub <- " "
  if (!is.null(right$sub)) right$sub <- " "
  right <- emptyLeftAxisLeftStrip(right)


  middle$xlab <- " "

  
  ##            scales=list(y=list(tck=0)),
  middle$par.settings$axis.line$col <- "transparent"
  middle$panel <- function(...) {}
  middle$xlab <- " "
  if (!is.null(middle$xlab.top)) middle$xlab.top <- " "
  middle$x.limits[] <- " "
  middle <- update(middle, par.settings=list(
                    layout.widths=list(
                      left.padding=1,     
                      key.left=0,         
                      key.ylab.padding=0, 
                      ylab=0,             
                      ylab.axis.padding=0,
                      axis.left=0,        
                      axis.panel=0,       
                      strip.left=0,       
                      panel=1,            
                      between=0,          
                      axis.right=0,       
                      axis.key.padding=0, 
                      ylab.right=0,       
                      key.right=0,        
                      right.padding=1
                      )))
  middle <- emptyLeftAxisLeftStrip(middle)
  middle$plot.args$panel.width <- list(x=0.001, units="mm", data=NULL)

  if (keepLegend) {
    left$legend$bottom$args  <- emptyLegend( left$legend$bottom$args)
    right$legend$bottom$args <- emptyLegend(right$legend$bottom$args)
    ## middle$legend$bottom$args <- left$legend$bottom$args ## not needed
  } else {
    left$legend$bottom   <- NULL
    right$legend$bottom  <- NULL
    middle$legend$bottom <- NULL
  }

  result <- list(left=left, middle=middle, right=right)
  attr(result, "px") <- px
  class(result) <-"TwoTrellisColumns"
  result
}


print.TwoTrellisColumns <- function(x, px=attr(x, "px"), ...) {
  print(x$left,   position=c(px$L[1], 0, px$L[2], 1), more=TRUE)
  print(x$middle, position=c(px$M[1], 0, px$M[2], 1), more=TRUE)
  print(x$right,  position=c(px$R[1], 0, px$R[2], 1), more=FALSE)
  invisible(x)
}


emptyLeftAxisLeftStrip <- function(x) {
  ## left tick labels
  if (is.list(x$y.limits))
    x$y.limits <- lapply(x$y.limits,
                         function(x) {
                           x[] <- ""
                           x
                         }
                         )
  else
    x$y.limits[] <- " "
  x$ylab <- ""
  ## left strip
  x$strip.left <- FALSE
  x$par.strip.text$lines <- 0
  x
}
## source("c:/HOME/rmh/HH-R.package/HH/R/print.TwoTrellisColumns.R")
