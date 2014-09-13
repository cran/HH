rbind.trellis <- function(..., deparse.level=1,
                          combineLimits=TRUE, useOuterStrips=TRUE) {
  dddots <- list(...)
  dimdddots <- sapply(dddots, dim)
  if (is.list(dimdddots) || length(dim(dimdddots)) > 1)
    stop("Only one-dimensional trellis objects can be used in rbind.trellis or cbind.trellis.",
         call.=FALSE)
  if (any(dimdddots != dimdddots[1]))
    stop("All one-dimensional trellis objects in rbind.trellis or cbind.trellis must have the same dim value.",
         call.=FALSE)
  cdddots <- do.call(c, c(dddots, list(layout=c(dim(dddots[[1]]), length(dddots)))))
  dddnames <- names(dddots)
  if (is.null(dddnames)) dddnames <- LETTERS[1:length(dddots)]
  cdddots$condlevels <- list(dddots[[1]]$condlevels[[1]], dddnames)
  cdddots$index.cond <- list(1:length(dddots[[1]]$condlevels[[1]]), 1:length(dddots))
  cdddots$perm.cond <- 1:2
  dim(cdddots$packet.sizes) <- dim(cdddots)
  dimnames(cdddots$packet.sizes) <- dimnames(cdddots)
  if (useOuterStrips) cdddots <- useOuterStrips(cdddots)
  if (combineLimits) cdddots <- combineLimits(cdddots)
  cdddots
}
