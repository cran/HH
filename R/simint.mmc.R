"simint.mmc" <-
  function(y, data, type="Tukey",
           lmat=NULL, lmat.rows=2:nrow(mca.lmat),
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           whichf, cmatrix=t(lmat), covariates, ...) {

    if.R(s=stop("simint.mmc works only in R.  Use multcomp.mmc in S-Plus."),
         r={})
 
    if (missing(whichf) && (class(y[[3]])=="name")) {
      whichf <- as.character(y[[3]])
      }
    aov.mca <- aov(y, data, x=TRUE)
    aov.mca.x <- aov.mca$x
    x.names <- dimnames(aov.mca.x)[[2]]
    coef.mca <- coef(aov.mca)
    if (!missing(covariates)) {
      cov.names <- x.names[match(covariates, x.names, 0)]
      cov.means <- apply(aov.mca.x[,cov.names, drop=FALSE], 2, mean)
      for (i in cov.names) aov.mca.x[,i] <- cov.means[i]
    }
    means <- tapply(aov.mca.x %*% coef.mca, data[[whichf]], mean)
    ## means <- model.tables(aov.mca, type="means", focus=whichf)$tables[[whichf]]
    mca.simint <- simint(y, data=data, type=type,
                         whichf=whichf, ...)
    mca.simint <- simint.reverse(mca.simint, estimate.sign)
    mca.lmat <- t(mca.simint$cmatrix)
    mca.lmat.rows <- lmat.rows
    mca <- as.multicomp(mca.simint, aov.mca=aov.mca, means=means,
                        lmat=mca.lmat, lmat.rows=mca.lmat.rows)

    lhs <- rbind(1, diag(ncol(mca.simint$cmatrix)-1))
    rhs <- cbind(0, t(mca.simint$cmatrix))
    rhs[1,1] <- 1

    y.none <- y
    y3 <- ~ a - 1
    y3[[2]][[2]] <- y.none[[3]]
    y.none[[3]] <- y3[[2]]
    
    lmat.none <- diag(ncol(mca.simint$cmatrix)-1)
    dimnames(lmat.none) <- list(names(means), names(means))
    none.simint <- simint(y.none, data=data, whichf=whichf,
                          cmatrix=lmat.none, ...)
    none <- as.multicomp(none.simint, aov.mca=aov.mca, means=means,
                              lmat.rows=mca.lmat.rows-1)
    
    result <- list(mca=mca, hmtest=mca.simint,
                   none=none, none.hmtest=none.simint)
    class(result) <- "mmc.multicomp"

    if (order.contrasts) {
      result$none <- multicomp.order(result$none)
      result$mca <- multicomp.order(result$mca)
  }
    if (missing(lmat) && missing(cmatrix)) return(result)
    
    ## user-specified lmat
    if (!missing(cmatrix) && missing(lmat)) lmat <- t(cmatrix)
    
    lmat.subscript <- lmat[lmat.rows,,drop=FALSE]
    if (lmat.scale.abs2) {
      lmat <- sweep(lmat, 2, apply(abs(lmat.subscript), 2, sum)/2, "/")
      cmatrix <- t(lmat)
    }
    lmat.simint <- simint(y, data=data,
                          whichf=whichf, cmatrix=cmatrix, ...)
    lmat.simint <- simint.reverse(lmat.simint, estimate.sign)
    lmat.multicomp <- as.multicomp(lmat.simint, aov.mca=aov.mca, means=means,
                                   lmat=lmat, lmat.rows=lmat.rows)
    result$lmat <- lmat.multicomp
    if (order.contrasts) result$lmat <- multicomp.order(result$lmat)
    result$lmat.hmtest <- lmat.simint
    
    result
  }
