## The catalystm data comes from Montgomery (1997)
## Montgomery, D. C. (1997).
## Design and Analysis of Experiments, Wiley, 4th edition.



catalystm <- read.table(hh("datasets/catalystm.dat"), header=FALSE,
                       col.names=c("catalyst","concent"))
catalystm$catalyst <- factor(catalystm$catalyst, labels=c("A","B","C","D"))
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
class(catalystm1.aov) <- rev(class(catalystm1.aov))
catalystm.mca <-
   if.R(r=simint(concent ~ catalyst, data=catalystm, type="Tukey"),
        s=multicomp(catalystm1.aov))

catalystm.lmat <- cbind("AB-D" =c(0, 1, 1, 0,-2),
                        "A-B"  =c(0, 1,-1, 0, 0),
                        "ABD-C"=c(0, 1, 1,-3, 1))
dimnames(catalystm.lmat)[[1]] <-
if.R(s=dimnames(catalystm.mca$lmat)[[1]],
     r=c("(Intercept)", dimnames(catalystm.mca$cmatrix)[[2]][-1]))

if.R(r={
        catalystm.mmc <- simint.mmc(concent ~ catalyst, data=catalystm,
                          lmat=catalystm.lmat, lmat.rows=2:5,
                          type="Tukey", whichf="catalyst")
        catalystm.mmc <- multicomp.label.change(catalystm.mmc, "catalyst", "")
       },
     s={
        catalystm.mmc <- multicomp.mmc(catalystm1.aov, lmat=catalystm.lmat,
                                       plot=FALSE)
       })

gray       <- if.R(r="darkgray", s=16)
red        <- if.R(r="red",  s=8)
blue       <- if.R(r="blue", s=6)
lty.contr0 <- if.R(r=3, s=2)
lty.iso    <- if.R(r=2, s=8)

old.mar <- par(mar=c(5,6,4,4)+.1)

## MMC Figure 1, pairwise contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     print.lmat=FALSE,
     col.mca.signif=red, lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0=gray,
     lty.iso=lty.iso, col.iso=gray)

## MMC Figure 5, user-specified contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     col.mca.signif=red, lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0=gray,
     lty.iso=lty.iso, col.iso=gray,
     col.lmat.signif=blue)

## both pairwise contrasts and user-specified contrasts
plot(catalystm.mmc, x.offset=1.6, ry.mmc=c(50.5,57),
     print.mca=TRUE,
     col.mca.signif=red, lty.mca.not.signif=4,
     lty.contr0=lty.contr0, col.contr0=gray,
     lty.iso=lty.iso, col.iso=gray,
     col.lmat.signif=blue)

par(old.mar)

## See the help file for plot.mmc.multicomp for more examples



## illustrate the construction of the isomeans grid and the contrasts
source(hh("scripts/MMC.mmc.explain.s"))  ## mmc.jcs.explain() and tpg.col()

group <- levels(catalystm$catalyst)
n <- c(4,4,4,4)
ybar <- tapply(catalystm$concent, catalystm$catalyst, mean)
ms.5 <- if.R(s=summary(catalystm1.aov)$"Mean Sq"[2]^.5,
             r=summary(catalystm1.aov)$sigma)
crit.point <- catalystm.mmc$mca$crit.point

gray  <- if.R(r="darkgray", s=65)
black <- if.R(r="black",    s=1)


## With these four calls to mmc.jcgs.explain,
## each of the stopping points is colored differently.

## trellis.device.hh.bw()  ## recommendation, S-Plus only
if.R(s=old.oma <- par(oma=c(-1,-3,-4,-8)),
     r={})

## MMC Figure 2a, isomeans grid
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=1, col.in=c(black,black,black))

## move m-axis and project group means (A illustrated) to m-axis.
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=4, col.in=c(gray,black,black))

## MMC Figure 2b
##     move m-axis and d-axis.
##     project group means (A illustrated) to m-axis.
##     project differences (A-D illustrated) to d-axis.
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.6, cex.tick=1.3,
                 exit=2, col.in=c(gray,black,black))

## confidence interval for A-B difference
mmc.jcgs.explain(group, n, ybar, ms.5, crit.point,
                 ylabel="concent", method="tukey",
                 xlim=c(46,62), cex.lab=1.5, cex.tick=1.3,
                 exit=3, col.in=c(gray,gray,black))

par(col=black) ## restore
tpg.col(black) ## restore

if.R(s=par(old.oma),
     r={})
