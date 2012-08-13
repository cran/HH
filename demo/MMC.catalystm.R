## The catalystm data comes from Montgomery (1997)
## Montgomery, D. C. (1997).
## Design and Analysis of Experiments, Wiley, 4th edition.



data(catalystm)
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)

catalystm.mmc <-
if.R(r=
     mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
    ,s=
     multicomp.mmc(catalystm1.aov, plot=FALSE)
)

old.mar <- if.R(s=par(mar=c(5,8,4,4)+.1),
                r=par(mar=c(12,4,4,3)+.1))
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     print.lmat=FALSE)

catalystm.lmat <- cbind("AB-D" =c( 1, 1, 0,-2),
                        "A-B"  =c( 1,-1, 0, 0),
                        "ABD-C"=c( 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <- levels(catalystm$catalyst)

catalystm.mmc <-
if.R(r=
     mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"),
              focus.lmat=catalystm.lmat)
     ,s=
     multicomp.mmc(catalystm1.aov, focus.lmat=catalystm.lmat,
                   plot=FALSE)
)

plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57))



lty.contr0 <- if.R(r=3, s=2)
lty.iso    <- if.R(r=2, s=8)

## MMC Figure 1, pairwise contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     print.lmat=FALSE,
     col.mca.signif='red', lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0='darkgray',
     lty.iso=lty.iso, col.iso='darkgray')

## MMC Figure 5, user-specified contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     col.mca.signif='red', lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0='darkgray',
     lty.iso=lty.iso, col.iso='darkgray',
     col.lmat.signif='blue')

## both pairwise contrasts and user-specified contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     print.mca=TRUE,
     col.mca.signif='red', lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0='darkgray',
     lty.iso=lty.iso, col.iso='darkgray',
     col.lmat.signif='blue')

par(old.mar)

## See the help file for plot.mmc.multicomp for more examples



## illustrate the construction of the isomeans grid and the contrasts
source(hh.old("demo/MMC.mmc.explain.R"))  ## mmc.jcs.explain() and tpg.col()

group <- levels(catalystm$catalyst)
n <- c(4,4,4,4)
ybar <- tapply(catalystm$concent, catalystm$catalyst, mean)
ms.5 <- if.R(s=summary(catalystm1.aov)$"Mean Sq"[2]^.5,
             r=summary(catalystm1.aov)[[1]]$"Mean Sq"[2]^.5)
crit.point <- catalystm.mmc$mca$crit.point


## With these four calls to mmc.jcgs.explain,
## each of the stopping points is colored differently.

## trellis.device.hh.bw()  ## recommendation, S-Plus only
if.R(s={old.oma <- par(oma=c(-1,-3,-4,-8))
        par.settings <- NULL},
     r=par.settings <-
       list(layout.widths=list(left.padding=15, axis.key.padding=0),
            layout.heights=list(bottom.padding = 0))
)

## MMC Figure 2a, isomeans grid
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=1, col.in=c('black','black','black'),
                 par.settings=par.settings)

## move m-axis and project group means (A illustrated) to m-axis.
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=4, col.in=c('darkgray','black','black'),
                 par.settings=par.settings)

## MMC Figure 2b
##     move m-axis and d-axis.
##     project group means (A illustrated) to m-axis.
##     project differences (A-D illustrated) to d-axis.
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.6, cex.tick=1.3,
                 exit=2, col.in=c('darkgray','black','black'),
                 par.settings=par.settings)

## confidence interval for A-B difference
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=3, col.in=c('darkgray','darkgray','black'),
                 par.settings=par.settings)

par(col='black') ## restore
tpg.col('black') ## restore

if.R(s=par(old.oma),
     r={})
