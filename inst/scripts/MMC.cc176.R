## The cc176 data comes from Cochran and Cox, page 176
## Cochran, W. G. and Cox, G. M. (1957).
## Experimental Designs, Wiley.

cc176 <- matrix(scan(hh("datasets/cc176.dat")), ncol=2, byrow=TRUE)

current.levels <- c("galvanic","faradic","60.cycle","25.cycle")
cc176 <-
  data.frame(wt.d=cc176[,1],
             wt.n=cc176[,2],
             n.treats=ordered(rep(c(1,3,6), length=96)),
             current=ordered(rep(rep(current.levels,
               rep(3,4)),8), levels=current.levels),
             minutes=ordered(rep(rep(c(1,2,3,5),rep(12,4)), 2)),
             rep=rep(c("I","II"), c(48,48)))
contrasts(cc176$n.treats)
contrasts(cc176$n.treats) <-
  if.R(s=
       contr.poly(c(1,3,6))
       ,r=
       contr.poly(3, scores=c(1,3,6))
       )
contrasts(cc176$n.treats)

cc176.aov <- aov(wt.d ~ rep + wt.n + n.treats + wt.n*current, data=cc176)
summary(cc176.aov)

summary(cc176.aov,
        split=list(n.treats=list(n.treats.lin=1, n.treats.quad=2)),
        expand.split=FALSE)



## almost MMC Figure 3, mmc plot of current, default settings for display
if.R(s={
  old.par <- par(mar=c(5,4,4,8)+.1)
  cc176.lmat.mca <- multicomp(cc176.aov, focus="current")$lmat
  print(dimnames(cc176.lmat.mca)[[1]])  ## from which we see we need 8:11
  print(zapsmall(cc176.lmat.mca[8:11,]))
  cc176.mmc <-
    multicomp.mmc(cc176.aov, lmat.rows=8:11, focus="current",
                  ry=c(56,72), x.offset=1.5,
                  valid.check=FALSE, ## Tukey method (slightly narrower bounds)
                  plot=FALSE)
  print(cc176.mmc)
  plot(cc176.mmc)
  par(old.par)
}
,r={
  warning("simint in multcomp version 0.4-8 doesn't handle covariates correctly.")

  warning("cc176.partial.mmc is an approximation to the correct display")
  cc176.proj <- proj(cc176.aov)
  dimnames(cc176.proj)[[2]]
  wt.d.partial <- apply(cc176.proj[, c(1,5,7)], 1, sum)
  wt.d.partial
  
  cc176.partial.aov <- aov(wt.d.partial ~ current, data=cc176)
  summary(cc176.partial.aov)
  cc176.partial.aov$df.residual <- 85
  summary(cc176.partial.aov)

  cc176.partial.mmc <-
    if.R(r=simint.mmc(wt.d.partial ~ current, data=cc176),
         s=multicomp.mmc(cc176.partial.aov, plot=FALSE))
  if.R(r=cc176.partial.mmc <-
       multicomp.label.change(cc176.partial.mmc, "current", ""),
       s={})
  
  print(cc176.partial.mmc)
  plot(cc176.partial.mmc)
  
  ## simint in multcomp package version 0.4-8 doesn't handle
  ## covariates correctly.  I think the commands here will be correct
  ## when simint is repaired.
  ##
  ##   cc176.lmat.mca <-
  ##     t(simint(wt.d ~ rep + wt.n + n.treats + wt.n*current, data=cc176,
  ##              whichf="current", type="Tukey")$cmatrix)
  ##   print(dimnames(cc176.lmat.mca)[[1]])  ## from which we see we need 6:9
  ##   print(zapsmall(cc176.lmat.mca[6:9,]))
  ##   cc176.mmc <-
  ##     simint.mmc(wt.d ~ rep + wt.n + n.treats + wt.n*current,
  ##                data=cc176, whichf="current", covariates="wt.n",
  ##                lmat=cc176.lmat.mca, lmat.rows=6:9)
  ##   print(cc176.mmc)
  ##   plot(cc176.mmc)
})

## MMC Figure 3, mmc plot of current, control of display parameters
if.R(
     s={
       old.par <- par(mar=c(5,4,4,8)+.1)
       plot(cc176.mmc, iso.name=FALSE, col.iso=16,
            print.mca=TRUE, print.lmat=FALSE)
       par(old.par)
     },r={
       warning("cc176.partial.mmc is an approximation to the correct display")
       old.par <- par(mar=c(15,4,4,8)+.1)
       plot(cc176.partial.mmc,  x.offset=1.5,
            iso.name=FALSE, col.iso=16,
            col.mca.signif="red", lty.mca.not.signif=2,
            print.mca=TRUE, print.lmat=FALSE)
       par(old.par)
     })

     

## MMC Figure 4, current with orthogonal contrasts and control of display
if.R(s={
  cc176.lmat.orth <-
    cbind(cc176.lmat.mca[,c(1,6)],
          data.frame("gf-AC"=cc176.lmat.mca[,2:5] %*% c(1,1,1,1),
                     check.names=FALSE))
  zapsmall(cc176.lmat.orth[8:11,])
  cc176.mmc <-
    multicomp.mmc(cc176.aov, lmat.rows=8:11, focus="current",
                  lmat=cc176.lmat.orth,
                  ry=c(56,72), x.offset=1.5,
                  valid.check=FALSE, ## Tukey method
                  plot=FALSE)
  print(cc176.mmc)
  old.par <- par(mar=c(5,4,4,8)+.1)
  plot(cc176.mmc, iso.name=FALSE, col.iso=16,
       print.mca=FALSE, print.lmat=TRUE,
       col.lmat.signif=6)
  par(old.par)
}
,r={
  warning("simint in multcomp version 0.4-8 doesn't handle covariates correctly.")

  warning("cc176.partial.mmc is an approximation to the correct display")
  
  cc176.lmat.orth <-
    cbind(cc176.partial.mmc$mca$lmat[,c(1,6)],
          data.frame("gf-AC"=-cc176.partial.mmc$mca$lmat[,2:5] %*%
                     c(1,1,1,1),
                     check.names=FALSE))
  cc176.partial.mmc <-
    if.R(r=simint.mmc(wt.d.partial ~ current, data=cc176,
           cmatrix=t(cc176.lmat.orth)),
         s=multicomp.mmc(cc176.partial.aov,
           lmat=cc176.lmat.orth,
           plot=FALSE))
  if.R(r=cc176.partial.mmc <-
       multicomp.label.change(cc176.partial.mmc, "current", ""),
       s={})
  print(cc176.partial.mmc)
  warning("cc176.partial.mmc is an approximation to the correct display")
  old.par <- par(mar=c(15,4,4,8)+.1)
  plot(cc176.partial.mmc, x.offset=1.5,
       iso.name=FALSE, col.iso=16,
       col.mca.signif="red", lty.mca.not.signif=2,
       col.lmat.signif="blue", lty.lmat.not.signif=2,
       print.mca=FALSE, print.lmat=TRUE)
  par(old.par)
  })



## MMC Figure 10, current, pairwise and orthogonal contrasts
if.R(
     s={
       old.par <- par(mar=c(5,4,4,8)+.1)
       plot(cc176.mmc, iso.name=FALSE, col.iso=16,
            print.mca=TRUE, print.lmat=TRUE,
            col.lmat.signif=6)
       par(old.par)
     },r={
       warning("cc176.partial.mmc is an approximation to the correct display")
       old.par <- par(mar=c(15,4,4,8)+.1)
       plot(cc176.partial.mmc,  x.offset=1.5,
            iso.name=FALSE, col.iso=16,
            col.mca.signif="red", lty.mca.not.signif=2,
            col.lmat.signif="blue", lty.lmat.not.signif=2,
            print.mca=TRUE, print.lmat=TRUE)
       par(old.par)
     })
