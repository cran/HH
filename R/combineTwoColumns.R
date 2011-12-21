combineTwoTrellisColumns <- function(LLLRRR, w=c(1,1), strip.lines=.8, strip.cex=.7, ...) {
  dim.LLLRRR <- dim(LLLRRR)
  class.L <- class(LLLRRR[[1,1]])
  class.R <- class(LLLRRR[[2,1]])
  ## RRR.y.limits.right <- sapply(LLLRRR[2,], `[[`, "y.limits.right", simplify=FALSE)
  dimnames.LLLRRR <- dimnames(LLLRRR)
  names.LLLRRR <- do.call("outer", c(dimnames(LLLRRR), list(paste, sep=".")))
  panel.heights <- as.vector(sapply(LLLRRR[1,], function(x) length(x$y.limits)))
  dimnames(LLLRRR) <- NULL
  dim(LLLRRR) <- NULL
  names(LLLRRR) <- names.LLLRRR

  LLLRRRc <- do.call("c", c(LLLRRR, list(layout=dim.LLLRRR)))
  LLLRRRc <- update(LLLRRRc, xlab=dimnames.LLLRRR[[1]], ...)

  LLLRRRcm <- LLLRRRc
  CriteriaNames <- dimnames.LLLRRR[[2]]
  LLLRRRcm$packet.sizes <- array(LLLRRRc$packet.sizes, dim.LLLRRR, dimnames=dimnames.LLLRRR)
  LLLRRRcm$index.cond <- lapply(dim.LLLRRR, seq, from=1)
  LLLRRRcm$perm.cond <- 1:2
  LLLRRRcm$condlevels <- dimnames.LLLRRR

  LLLRRRcm <- combineLimits(LLLRRRcm)
  LLLRRRcm$y.limits <- LLLRRRc$y.limits
  LLLRRRcm <- resizePanels(update(useOuterStrips(LLLRRRcm), xlab=NULL),
                           h=panel.heights, w=w)
  LLLRRRcm$par.strip.text$lines <- strip.lines
  LLLRRRcm$par.strip.text$cex <- strip.cex

  class(LLLRRRcm) <- class.R
  ## LLLRRRcm$y.limits.right <- RRR.y.limits.right
  LLLRRRcm$y.scale$alternating <- LLLRRR[[2]]$y.scale$alternating
  for (i in seq(1, 2*dim.LLLRRR[2], 2))
    LLLRRRcm$y.limits[[i]] <- LLLRRRcm$y.limits[[i+1]]
  ## LLLRRRcm
  update(LLLRRRcm, between=list(x=2))
}

LikertPercentCountColumns <- function(x, w=c(3,1), ...,
                                      strip.lines=.8, strip.cex=.8) {
  LLL <- sapply(rev(x), plot.likert, as.percent="noRightAxis", xlab="Percent", simplify=FALSE)
  RRR <- sapply(rev(x), plot.likert, rightAxis=TRUE, xlab="Count", simplify=FALSE)
  LLLRRR <- rbind(Percent=LLL, Count=RRR) ## rbind is needed
  combineTwoTrellisColumns(LLLRRR, w=w, ..., strip.lines=strip.lines, strip.cex=strip.cex)
}


### tests
if (FALSE) {
  library(HH)
  data(ProfChal)
  
  LLL <- sapply(ProfChal[6:1], plot.likert, as.percent="noRightAxis", xlab="Percent", simplify=FALSE)
  RRR <- sapply(ProfChal[6:1], plot.likert, rightAxis=TRUE, xlab="Count", simplify=FALSE)
##  LLL <- sapply(ProfChal[6:1], plot.likert, as.percent="noRightAxis", xlab="Percent", simplify=FALSE, yscale.components=yscale.components.right.HH)
##  RRR <- sapply(ProfChal[6:1], plot.likert, rightAxis=TRUE, xlab="Count", simplify=FALSE, yscale.components=yscale.components.right.HH)
  LLLRRR <- rbind(Percent=LLL, Count=RRR) ## rbind is needed
  dimnames(LLLRRR)[[2]][1] <- "Prof Recog"  ## original is "Attitude\ntoward\nProfessional\nRecognition"
  combineTwoTrellisColumns(LLLRRR, w=c(3,1), main="Is your job professionally challenging?", sub="combineTwoTrellisColumns test")
}

if (FALSE) {
  library(HH)
  data(ProfChal)
  
  ## Change name in the "Attitude" panel
  names(ProfChal)[6] <- "Prof Recog"
  
  LikertPercentCountColumns(ProfChal,
                            main="Is your job professionally challenging?",
                            sub="LikertPercentCountColumns test, 9x8 window")
  
  ## Restore original name
  names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"
}
