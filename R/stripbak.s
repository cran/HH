strip.background0 <- function() {
  s.b <- trellis.par.get("strip.background")
  s.b$col[] <- 0
  trellis.par.set("strip.background", s.b)
  s.s <- trellis.par.get("strip.shingle")
  s.s$col[] <- 0
  trellis.par.set("strip.shingle", s.s)
  invisible(NULL)
}
