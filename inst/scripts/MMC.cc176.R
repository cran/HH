## The cc176 data comes from Cochran and Cox, page 176
## Cochran, W. G. and Cox, G. M. (1957).
## Experimental Designs, Wiley.

if.R(r=
old.contr <- options(contrasts=c("contr.treatment", "contr.treatment"))
,s={})

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
if.R(r={
     contrasts(cc176$current) <- "contr.treatment"
     contrasts(cc176$n.treats) <- "contr.treatment"
   }
     ,s={})


if.R(r=
options(old.contr)
,s={})

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
  cc176.mca <-
    glht(cc176.aov, linfct=mcalinfct(cc176.aov, "current"))
  print(dimnames(cc176.mca$linfct)[[2]])  ## from which we see we need 6:8
  print(zapsmall(cc176.mca$linfct[,6:8]))

  cc176.mmc <-
    glht.mmc(cc176.aov, linfct=mcalinfct(cc176.aov, "current"), "current",
             lmat.rows=6:8)
  print(cc176.mmc)
  plot(cc176.mmc)
})


## MMC Figure 3, mmc plot of current, control of display parameters
if.R(
     s={
       old.par <- par(mar=c(5,4,4,8)+.1)
       plot(cc176.mmc, iso.name=FALSE, col.iso=16,
            print.mca=TRUE, print.lmat=FALSE)
       par(old.par)
     },r={
       old.par <- par(mar=c(15,4,4,8)+.1)
       plot(cc176.mmc,  x.offset=1.5,
            iso.name=FALSE, col.iso=16,
            col.mca.signif="red", lty.mca.not.signif=2,
            print.mca=TRUE, print.lmat=FALSE)
       par(old.par)
     })

     

## MMC Figure 4, current with orthogonal contrasts and control of display
if.R(s={
  cc176.lmat.orth <-
    cbind(cc176.lmat.mca[,c(1,6)],
         cc176.lmat.mca[,2:5] %*% c(1,1,1,1))
   dimnames(cc176.lmat.orth)[[2]][3] <- "gf-AC"
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
            col.mca.signif="red", lty.mca.not.signif=2,
            print.mca=FALSE, print.lmat=TRUE)
  par(old.par)
}
,r={

  cc176.linfct.orth <-
    rbind(cc176.mca$linfct[c(1,6), ],
          "AC-gf"=apply(cc176.mca$linfct[2:5, ], 2, sum))
  zapsmall(cc176.linfct.orth[,6:8])
  cc176.mmc <-
    glht.mmc(cc176.aov, linfct=mcalinfct(cc176.aov, "current"), "current",
             lmat.rows=6:8, lmat=t(cc176.linfct.orth))
  print(cc176.mmc)


  old.par <- par(mar=c(15,4,4,8)+.1)
  plot(cc176.mmc, x.offset=1.5,
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
       old.par <- par(mar=c(15,4,4,8)+.1)
       plot(cc176.mmc,  x.offset=1.5,
            iso.name=FALSE, col.iso=16,
            col.mca.signif="red", lty.mca.not.signif=2,
            col.lmat.signif="blue", lty.lmat.not.signif=2,
            print.mca=TRUE, print.lmat=TRUE)
       par(old.par)
     })
