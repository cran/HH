## calculate multiple comparisons from sufficient statistics
simint.mean <- function(y, n, ybar, s, alpha=.05,
                        ylabel="ylabel", focus.name="focus.factor", plot=FALSE,
                        lmat, labels=NULL,
                        method="Tukey",
                        bounds="both",
                        df=sum(n) - length(n),
                        sigmahat=(sum((n-1)*s^2) / df)^.5,
                        contrasts, ..., group=y) {

  ## based on ?cholesterol in the multcomp package in R

  if.R(s=stop("simint.mean works only in R.  Use multcomp.mean in S-Plus."),
       r={})
  
  if (method=="tukey") method <- "Tukey"
  if (method=="dunnett") method <- "Dunnett"
  
  alternative <- switch(bounds,
                        both="two.sided",
                        lower="greater",
                        upper="less",
                        bounds)

  names(n) <- group
  cmatrix <- if (missing(lmat)) {
    if (method=="Dunnett" && !missing(contrasts)) {
      tmp <- contr.Dunnett(n, contrasts=contrasts)
      dimnames(tmp) <- list(group, group)
      tmp
    }
    else
      contrMat(n, type=method)
  }
  else
    t(lmat)

  if (!(
        missing(lmat) && method=="Dunnett" &&
        !missing(contrasts) && !contrasts
        )) {
    lmat <- t(cmatrix)
    lmat <- sweep(lmat, 2, apply(abs(lmat), 2, sum)/2, "/")
    cmatrix <- t(lmat)
  }
  
  csimint(estpar=ybar, df=df, covm=diag(sigmahat^2/n),
          cmatrix=cmatrix,
          ctype=method, alternative=alternative,
          conf.level=1-alpha)
}

## trace(simint.mean, exit=recover)


simint.mmc.mean <- function(y, n, ybar, s, alpha=.05,
                            ylabel="ylabel", focus.name="focus.factor", plot=FALSE,
                            lmat, labels=NULL,
                            method="Tukey",
                            bounds="both",
                            df=sum(n) - length(n),
                            sigmahat=(sum((n-1)*s^2) / df)^.5,
                            estimate.sign=1,
                            order.contrasts=TRUE, ..., group=y) {

  if.R(s=stop("simint.mmc.mean works only in R.  Use multcomp.mmc.mean in S-Plus."),
       r={})

  mca.simint <- simint.mean(group,
                            n,
                            ybar,
                            s,
                            ylabel=ylabel,
                            focus=focus.name)
  
  none.simint <- simint.mean(group,
                             n,
                             ybar,
                             s,
                             ylabel=ylabel,
                             focus=focus.name,
                             method="Dunnett", contrasts=FALSE)
  
  result <- list(mca=as.multicomp(mca.simint, means=ybar,
                   lmat.rows=seq(along=group),
                   ylabel=ylabel,
                   focus=focus.name),
                 hmtest=mca.simint,
                 none=as.multicomp(none.simint, means=ybar,
                   lmat.rows=seq(along=group),
                   ylabel=ylabel,
                   focus=focus.name),
                 none.hmtest=none.simint)

  if (!missing(lmat)) {
    lmat.simint <- simint.mean(row.names(pulmonary),
                               pulmonary$n,
                               pulmonary$ybar,
                               pulmonary$s,
                               ylabel=ylabel,
                               focus=focus.name,
                               lmat=lmat)
    result$lmat <- as.multicomp(lmat.simint, means=ybar,
                                lmat.rows=seq(along=group),
                                ylabel=ylabel,
                                focus=focus.name)
    result$lmat.hmtest <- lmat.simint
  }

  if (order.contrasts) {
    result$mca <- multicomp.order(result$mca)
    result$none <- multicomp.order(result$none)
    if (!is.null(result$lmat))
      result$lmat <- multicomp.order(result$lmat)
  }
  if (estimate.sign != 0) {
    result$mca <- multicomp.reverse(result$mca, estimate.sign)
    result$none <- multicomp.reverse(result$none, estimate.sign)
    if (!is.null(result$lmat))
      result$lmat <- multicomp.reverse(result$lmat, estimate.sign)
  }
  
  class(result) <- "mmc.multicomp"
  result
}
