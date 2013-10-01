## The default line width for residplot (which is called by tsdiagplot)
## and for seqplot is lwd=0.  This looks ok on screen.  When it is
## exported from screen to postscript the line is rendered in a light
## gray.  Springer darkened and increased the weight of the line so that
## it dominated the graph.  Therefore for the figures in the book, we use
## the postscript driver directly and use the value lwd=.5.  This gives a
## lightweight black line.

## For example, we indicate in file tser/code/tsamstat.s the export command
##              ## export.eps(hh("tser/figure/tsamsta4.ps"))
## What we actually do for the book is


if (FALSE) {
  ## trellis.device(file=hh.old("tser/figure/tsamsta4.ps"), postscript, horizontal=TRUE)
  trellis.device(file="tsamsta4.ps", postscript, horizontal=TRUE) ## current getwd()
  strip.background0()
  tsdiagplot(armas=ddco2.loopPQ, diags=ddco2.diagsPQ,
             lag.lim=c(-2,38),
             lag.x.at=seq(0,36,6),
             lag.x.labels=c(0,"",12,"",24,"",36),
             main="", lwd=.5)
  dev.off()
}
