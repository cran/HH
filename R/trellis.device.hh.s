## These functions are for S-Plus only.  They are not for R.
##
## S-Plus scoping during package installation makes it necessary to
## define the function for both languages and then remove it from R.
## S-Plus scoping during installation won't allow function definition
## inside the if.R() statement.  Therefore we always assign the
## function, and then remove it from the R package.

trellis.device.hh.bw <- function(color=F, ...) {
  trellis.device(color=color, ...)
  strip.background0()
  tpgss <- trellis.par.get("superpose.symbol")
  tpgss$pch[c(1,6)] <- tpgss$pch[c(6,1)] ##Interchange BW open and closed circle
  tpgss$cex[c(1,6)] <- tpgss$cex[c(6,1)] ##Interchange BW open and closed circle
  trellis.par.set("superpose.symbol", tpgss)
  tpgps <- trellis.par.get("plot.symbol")
  tpgps$pch <- 16 ## closed circle
  trellis.par.set("plot.symbol", tpgps)
}

trellis.device.hh.color <- function(color.scheme="standard", ...) {
  trellis.device(color.scheme=color.scheme, ...)
  strip.background0()
}

trellis.device.hh.ps.bw <- function(file=NULL, horizontal=T, ...) {
  ps.options(reset=T)
  trellis.device(postscript, file=file, horizontal=horizontal, ...)
  strip.background0()
}

trellis.device.hh.ps.color <- function(file=NULL, horizontal=T, ...) {
  ps.options(colors=colorps.trellis)
  trellis.device(postscript, file=file, horizontal=horizontal, ...)
  strip.background0()
}

trellis.device.hh.color.jsm <- function(color.scheme="standard", ..., sb0=T) {
  trellis.device(color.scheme=color.scheme, ...)
  if (sb0) strip.background0()
  tpgss <- trellis.par.get("superpose.symbol")
  tpgss$pch[] <- 16 ##Interchange BW open and closed circle
  tpgss$cex[] <- .7 ##Interchange BW open and closed circle
  trellis.par.set("superpose.symbol", tpgss)
  tpgps <- trellis.par.get("plot.symbol")
  tpgps$pch <- 16 ## closed circle
  tpgps$cex <- .7 ## closed circle
  trellis.par.set("plot.symbol", tpgps)
}

## Remove it from the R package.
##
if.R(s={},
     r=remove(list=c(
                "trellis.device.hh.bw",
                "trellis.device.hh.color",
                "trellis.device.hh.ps.bw",
                "trellis.device.hh.ps.color",
                "trellis.device.hh.color.jsm")))
