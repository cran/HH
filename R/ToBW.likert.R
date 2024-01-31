ToBW.likert <- function(x,
                        colLegendOrder=c("gray70", "gray20", "gray60", "gray75", "gray45"),
##                                        ^Ask      Refu      ^Imp    | Impt      Essn
##
                        colBarchartOrder=colLegendOrder[c(3,2,1, 4,5)],
##                                        ^Imp      Refu      ^Ask    | Impt      Essn
                        columns=5) {
  ## negative colors are in reverse order in the BarchartOrder
  ## x is the R object for a figure.
  x$legend$bottom$args$columns  <- columns
  x$legend$bottom$args$rect$col <- colLegendOrder
  x$panel.args.common$col       <- colBarchartOrder
  x$panel.args.common$border    <- colBarchartOrder
  x
}
