## .First.lib <- function(lib, pkg) {
.onLoad <- function(libname, pkgname) {
    options(HH.ROOT.DIR=paste(libname, pkgname, sep="/"))
}
