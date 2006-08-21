"strip.interaction2wt" <-
function(which.given,
                                 which.panel,
                                 var.name,
                                 factor.levels,
                                 shingle.intervals,
##                                 par.strip.text=trellis.par.get("add.text"),
                                 strip.names=c(TRUE,TRUE),
                                 style=1,
                                 ...) {
  if(which.panel[1] == 1) return()
  strip.default(which.given=which.given,
                which.panel=which.panel,
                var.name=var.name,
                factor.levels=factor.levels,
                shingle.intervals=shingle.intervals,
##                par.strip.text=par.strip.text,
                strip.names=strip.names,
                style=style,
                ...)
}

