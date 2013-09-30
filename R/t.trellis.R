"t.trellis" <- function(x) {
  if.R(
       r={
         ## lattice:::t.trellis(x)
         lattice.t.trellis(x)
       },s={
         names.x <- names(x)
         n.x.index <-
           match(c("x","y","xlab","ylab","xlim","ylim","adj.xlim","adj.ylim"),
                 names.x, nomatch=0)
         names(x)[n.x.index] <-
           c("y","x","ylab","xlab","ylim","xlim","adj.ylim","adj.xlim"
             )[n.x.index != 0]
         n.x.s <- names(x$scales)
         nxsx <- match("x", n.x.s, 0)
         nxsy <- match("y", n.x.s, 0)
         n.x.s[nxsx] <- "y"
         n.x.s[nxsy] <- "x"
         names(x$scales) <- n.x.s

         x$labs[1:2] <- x$labs[2:1]

         if (is.function(x$panel)) {
           warning("x$panel is a function, this transpose may not work correctly.")
           if (is.null(x$transpose))
             x$transpose <- TRUE
           else
             x$transpose <- !x$transpose
           return(x)
         }

         if (x$panel=="panel.bwplot.intermediate.hh" ||
             x$panel=="panel.bwplott" ||
             x$panel=="panel.barchartt" ||
             x$panel=="panel.dotplott" ||
             x$panel=="panel.stripplot" ||
             x$panel=="panel.ancova" ||
             x$panel=="panel.histogram.hh") {
           if (is.null(x$transpose))
             x$transpose <- TRUE
           else
             x$transpose <- !x$transpose
         }

         if (x$panel=="panel.bwplot") {
           x$panel <- "panel.bwplott"
           x$transpose <- TRUE
         }

         if (x$panel=="panel.barchart") {
           x$panel <- "panel.barchartt"
           x$transpose <- TRUE
         }

         if (x$panel=="panel.dotplot") {
           x$panel <- "panel.dotplott"
           x$transpose <- TRUE
         }

         if (x$panel=="panel.histogram") {
           x$panel <- "panel.histogram.hh"
           x$transpose <- TRUE
         }

         x
       })
}
