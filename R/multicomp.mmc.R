multicomp.mmc <- function(..., comparisons="mca",
                          lmat, lmat.rows=-1,
                          lmat.scale.abs2=TRUE,
                          ry,
                          plot=TRUE,
                          crit.point,
                          iso.name=TRUE,
                          estimate.sign=1,
                          x.offset=0,
                          order.contrasts=TRUE,
                          main,
                          main2) {
  if.R(r=stop("multcomp.mmc works only in S-Plus.  Use simint.mmc in R."),
       s={})
  
  ## pairwise differences
  if (missing(crit.point)) {
    mc.mca=multicomp.lm(..., comparisons=comparisons, plot=FALSE)
    crit.point <- mc.mca$crit.point
  }
  else
    mc.mca=multicomp.lm(..., comparisons=comparisons,
      crit.point=crit.point, plot=FALSE)
    
  ## group means
  mc.none <- multicomp(..., plot=FALSE, comparisons="none", crit.point=crit.point)
  mc.none$method <- mc.mca$method
  mc.none$height <- mc.none$table[,"estimate"] * 2
  if (length(unlist(list(...)$adjust)) > 1) {
    warning("\nPlease verify that these two equivalent names of means
are in the same order.  If not, then change the order of positions
in the lmat.rows argument to match the groups column.\n")
    tmp <- cbind(groups=names(mc.none$table[,"estimate"]),
                 lmat.rows=dimnames(mc.mca$lmat[lmat.rows,])[[1]])
    dimnames(tmp)[[1]] <- seq(nrow(tmp))
    print(tmp, quote=FALSE)
  }
  mc.mca$height <- (mc.none$table[,"estimate"] %*%
                    abs(mc.mca$lmat[lmat.rows,]))[1,]
  if (estimate.sign != 0) mc.mca <- multicomp.reverse(mc.mca, estimate.sign)

  ## user-specified lmat
  if (!missing(lmat)) {
    if (lmat.scale.abs2)
      lmat <- sweep(lmat, 2, apply(abs(lmat[lmat.rows, , drop=FALSE]), 2, sum)/2, "/")
    mc.lmat <- multicomp(..., plot=FALSE, comparisons="none", crit.point=crit.point,
                         lmat=lmat)
    if (!is.null(mc.lmat$message)) stop(mc.lmat$message)
    mc.lmat$method <- mc.mca$method
    mc.lmat$height <- (mc.none$table[,"estimate"] %*% abs(lmat[lmat.rows,]))[1,]
    if (estimate.sign != 0) mc.lmat <- multicomp.reverse(mc.lmat, estimate.sign)
  }

  ## result
  result <- list(mca=mc.mca, none=mc.none)
  if (!missing(lmat)) result$lmat <- mc.lmat
  if (!missing(ry)) result$ry <- ry
  oldClass(result) <- c("mmc.multicomp", "list")

  if (order.contrasts) {
    result$none <- multicomp.order(result$none)
    result$mca <- multicomp.order(result$mca)
    if (!missing(lmat)) result$lmat <- multicomp.order(result$lmat)
  }

  if (!missing(main))  result$main  <- main
  if (!missing(main2)) result$main2 <- main2
  
  ## plot
  if (plot) plot.mmc.multicomp(result, iso.name=iso.name, x.offset=x.offset)

  return(result)
}

"[.mmc.multicomp" <- function(x, ..., drop = TRUE) {
 result <- NextMethod("[")
 oldClass(result) <- oldClass(x)
 result
}
