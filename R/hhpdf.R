## for use when running as an ordinary user
hhpdf <- function(file, ...) {invisible(NULL)}

hhdev.off <- function(...) {invisible(NULL)}

hhcapture <- function(file, text) {
  source(textConnection(text),
         echo=TRUE, print.eval=TRUE, keep.source=TRUE,
         max.deparse.length=500)
}

hhcode <- function(file, text) {
  cat(text)
}
