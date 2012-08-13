leftLabels.trellis <- function(x) {
  L <- x
  L$panel <- function(...) {}
  L$y.scales$alternating <- 1
  L <- update(L,
              par.settings=list(
                layout.widths=list(
                  axis.right=0,       
                  axis.key.padding=0, 
                  ylab.right=0,       
                  key.right=0,        
                  right.padding=0)))
  L$plot.args$panel.width <- list(x=0.001, units="mm", data=NULL)
  L$x.scales$labels[] <- " "
  L$par.settings$axis.line$col <- "transparent"
  if (!is.null(L$main)) L$main <- " "
  if (!is.null(L$xlab)) L$xlab <- " "
  if (!is.null(L$xlab.top)) L$xlab.top <- " "
  if (!is.null(L$legend$bottom$args))
    L$legend$bottom$args <- emptyLegend(L$legend$bottom$args)
  L
}
## tmp <- leftLabels.trellis(percentPlot)
## tmp

rightLabels.trellis <- function(x) {
  R <- x
  R$panel <- function(...) {}
  R$y.scales$alternating <- 2
  R <- update(R,
              par.settings=list(
                layout.widths=list(
                  left.padding=0,     
                  key.left=0,         
                  key.ylab.padding=0, 
                  ylab=0,             
                  ylab.axis.padding=0,
                  axis.left=0,        
                  axis.panel=0,       
                  strip.left=0,
                  between=0
                  )))
  R$plot.args$panel.width <- list(x=0.001, units="mm", data=NULL)
  R$x.scales$labels[] <- " "
  R$par.settings$axis.line$col <- "transparent"
  if (!is.null(R$main)) R$main <- " "
  if (!is.null(R$xlab)) R$xlab <- " "
  if (!is.null(R$xlab.top)) R$xlab.top <- " "
  if (!is.null(R$legend$bottom$args))
    R$legend$bottom$args <- emptyLegend(R$legend$bottom$args)
  R <- emptyLeftAxisLeftStrip(R)
  R
}
## tmp <- rightLabels.trellis(countPlot)
## tmp

panelOnly.trellis <- function(x) {
  P <- x
  P$y.scales$alternating <- 0
  P <- update(P,
              par.settings=list(
                layout.widths=list(
                  left.padding=0,     
                  key.left=0,         
                  key.ylab.padding=0, 
                  ylab=0,             
                  ylab.axis.padding=0,
                  axis.left=0,        
                  axis.panel=0,       
                  strip.left=0,
                  between=0,
                  axis.right=0,       
                  axis.key.padding=0, 
                  ylab.right=0,       
                  key.right=0,        
                  right.padding=0
                  )))
  if (!is.null(P$main)) P$main <- " "
  if (!is.null(P$legend$bottom$args))
    P$legend$bottom$args <- emptyLegend(P$legend$bottom$args)
  P <- emptyLeftAxisLeftStrip(P)
  P <- emptyRightAxis(P)
  P
}
## tmp <- panelOnly.trellis(percentPlot)
## tmp
## tmp <- panelOnly.trellis(countPlot)
## tmp

mainSubLegend.trellis <- function(x) {
  M <- x
  M$par.settings$axis.line$col <- "transparent"
  M$panel <- function(...) {}
  if (!is.null(M$xlab.top)) M$xlab.top <- " "
  M$x.limits[] <- " "
  M <- update(M, par.settings=list(
                    layout.widths=list(
                      left.padding=0,     
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
                      right.padding=0
                      )))
  M <- emptyLeftAxisLeftStrip(M)
  M <- emptyRightAxis(M)
  if (!is.null(M$xlab)) M$xlab <- " "
  if (!is.null(M$xlab.top)) M$xlab.top <- " "
  M$plot.args$panel.width <- list(x=0.001, units="mm", data=NULL)
  M
}
## tmp <- mainSubLegend.trellis(percentPlot)
## tmp

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

emptyRightAxis <- function(x) {
  ## right tick labels
  if (is.list(x$y.limits))
    x$y.limits <- lapply(x$y.limits,
                         function(x) {
                           x[] <- ""
                           names(x) <- NULL
                           x
                         }
                         )
  else
    x$y.limits[] <- " "
  x$ylab.right <- NULL
  x
}

as.TwoTrellisColumns5 <- function(left,  ## left  is the left trellis object
                                 right, ## right is the right trellis object
                                 ## Both left and right must have identical
                                 ## settings for number and size of vertical panels,
                                 ## left-axis labels, number of lines in main, sub, legend.
                                 ...,
                                 pw=c(.3, .30, .01, .30, .09),
                                 px=list(
                                   LL=c(0, pwc[1]),
                                   LP=pwc[1:2],
                                   ML=pwc[2:3],
                                   RP=pwc[3:4],
                                   RL=pwc[4:5]),
                                 pwc=cumsum(pw)
                                 ) {
  result <- list(LL=leftLabels.trellis(left),
                 LP=panelOnly.trellis(left),
                 ML=mainSubLegend.trellis(left),
                 RP=panelOnly.trellis(right),
                 RL=rightLabels.trellis(right))
  attr(result,"px") <- px
  class(result) <-"TwoTrellisColumns5"
  result
}

print.TwoTrellisColumns5 <- function(x, px=attr(x, "px"), ...) {
  print(x$LL, position=c(px$LL[1], 0, px$LL[2], 1), more=TRUE)
  print(x$LP, position=c(px$LP[1], 0, px$LP[2], 1), more=TRUE)
  print(x$ML, position=c(px$ML[1], 0, px$ML[2], 1), more=TRUE)
  print(x$RP, position=c(px$RP[1], 0, px$RP[2], 1), more=TRUE)
  print(x$RL, position=c(px$RL[1], 0, px$RL[2], 1), more=FALSE)
  invisible(x)
}


if (FALSE) {
## percentPlot and countPlot are defined in ?print.TwoTrellisColumns  
as.TwoTrellisColumns5(percentPlot, countPlot)  ## bad overlap for this example with 7in x 7in window

as.TwoTrellisColumns5(percentPlot, countPlot, ## acceptable in 7in x 7in
                     pw=c(.528, .186, .01, .186, .09)) ## These 5 numbers must sum to 1.

as.TwoTrellisColumns5(percentPlot, countPlot, ## better in 7in x 7in
                     px=list(
                       LL=c(0.000, 0.528),  ## 0.528 makes LL and LP touch without overlap        ## left labels and strip.left
                       LP=c(0.528, 0.714),                                                        ## left panel
                       ML=c(0.580, 0.581),  ## shifted left, to center main and key               ## main, sub, key from left argument
                       RP=c(0.724, 0.910),  ## gap from 0.714 to 0.724 puts space between columns ## right panel
                       RL=c(0.920, 1.000))) ## extra space                                        ## right labels
}
