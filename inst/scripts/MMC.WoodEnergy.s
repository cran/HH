## The energy data is from
##
## Milliken, G. A. and Johnson, D. E. (2002).
## Analysis of Messy Data Volume III: Analysis of Covariance,
## volume III, Chapman & Hall/CRC.

## The multiple comparsons in this file work in S-Plus.
##
## They don't work in R because simint in the multcomp package can't yet
## handle covariates.

energy <- if.R(r=read.table(hh("datasets/energy.dat"), header=TRUE, sep=","),
               s=importData(hh("datasets/energy.dat")))

energy$Wood <- ordered(energy$Wood,
                       levels=c(
                         "Osage Orange",
                         "Red Oak",
                         "Black Walnut",
                          "White Pine"))
                       
energy$Stove <- factor(energy$Stove, labels=c("A","B","C"))

energy <- energy[order(energy$Moist, energy$Wood, energy$Kind),]

tpg.sl.original <- trellis.par.get("superpose.line")
tpg.sl <- as.data.frame(tpg.sl.original)
if.R(r=tpg.sl$col <- tpg.sl.original$col, s={})
tpg.sl <- rbind(tpg.sl[1:6,], tpg.sl[1:6,])
tpg.sl$lty <- rep(c(1,5,8), 4)
if.R(r={tpg.sl <- unclass(tpg.sl)
        attr(tpg.sl,"row.names") <- NULL},
     s={})        
trellis.par.set("superpose.line", tpg.sl)

tpg.ss.original <- trellis.par.get("superpose.symbol")
tpg.ss <- as.data.frame(tpg.ss.original)
if.R(r={tpg.ss$col <- tpg.ss.original$col
        tpg.ss$fill <- tpg.ss.original$fill},
     s={})
tpg.ss <- rbind(tpg.ss[1:6,], tpg.ss[1:6,])
if.R(r={tpg.ss <- unclass(tpg.ss)
        attr(tpg.ss,"row.names") <- NULL},
     s={})
trellis.par.set("superpose.symbol", tpg.ss)

xyplot(Energy ~ Moist | Wood, groups=Stove, data=energy,
       panel=panel.superpose,
       type="b", pch=16,
       layout=c(4,1),
       par.strip.text=list(cex=1.2),
       key=list(border=T, title="Stove",
         text=list(levels(energy$Stove), col=tpg.sl$col[1:3]),
         lines=tpg.sl[1:3,]))

energy.abline <-
xyplot(Energy ~ Moist | Wood, groups=Stove, data=energy,
       panel=function(x, y, subscripts, groups, ...) {
         panel.superpose(x=x, y=y, subscripts=subscripts, groups=groups, ...)
         for (i in 1:length(levels(groups)))
           panel.abline(lm(y[groups[subscripts]==levels(groups)[i]] ~
                           x[groups[subscripts]==levels(groups)[i]]),
                        col=tpg.sl$col[i],
                        lty=tpg.sl$lty[i])
         },
       type="p", pch=c("A","B","C"),
       scales=list(cex=1, alternating=1),
       between=list(x=c(1,1,1)),
       layout=c(4,1),
       par.strip.text=list(cex=1.2),
       key=list(border=T, title="Stove",
         text=list(levels(energy$Stove), col=tpg.sl$col[1:3]),
         lines=tpg.sl[1:3,],
         space="right"))
print(energy.abline, position=c(-.05,.4, 1,1))
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.abline.eps"))



## MMC Figure 8
energy.am.abline <-
  xyplot(Energy ~ Moist | Wood, groups=Stove, data=energy,
         panel=function(x, y, subscripts, groups, ...) {
           panel.superpose(x=x, y=y, subscripts=subscripts, groups=groups, ...)
           for (i in 1:length(levels(groups))) {
             panel.abline(lm(y[groups[subscripts]==levels(groups)[i]] ~
                             x[groups[subscripts]==levels(groups)[i]]),
                          col=tpg.sl$col[i],
                          lty=tpg.sl$lty[i])
             panel.abline(v=14, lty=2)
           }
         },
         type="p", pch=c("A","B","C"),
         scales=list(cex=1, alternating=1),
         between=list(x=c(1,1,1)),
         layout=c(4,1),
         par.strip.text=list(cex=1.2),
         key=list(border=T, title="Stove",
           text=list(levels(energy$Stove),col=tpg.sl$col[1:3]),
           lines=tpg.sl[1:3,],
           space="right"))
print(energy.am.abline, position=c(-.05,.3, 1,1))
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am.abline.eps"))

energy.am.abline.2 <- energy.am.abline
energy.am.abline.2$key <- NULL
energy.am.abline.2$between$x[] <- 4
print(energy.am.abline.2, position=c(0,.6, 1,1))
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am.2.abline.eps"))




tpg.sl <- as.data.frame(tpg.sl.original)
if.R(r=tpg.sl$col <- tpg.sl.original$col, s={})
tpg.sl <- rbind(tpg.sl[1:3,], tpg.sl[1:3,], tpg.sl[1:3,], tpg.sl[1:3,])
trellis.par.set("superpose.line", tpg.sl)

energy.aov.4 <- aov(Energy ~ Moist + Stove*Wood + Moist:Stove:Wood,
                    data=energy)
anova(energy.aov.4)

if.R(r=warning("covariates don't work in simint.\nmultcomp is an S-Plus function."),
     s={})


## simulation method
energy.multicomp <- multicomp(energy.aov.4, focus="Stove",
                              adjust=list(Wood=c(
                                            "Osage Orange",
                                            "Red Oak",
                                            "Black Walnut",
                                            "White Pine")),
                              plot=FALSE)
energy.multicomp$crit.point
## [1] 2.957413

## Tukey method
energy.multicomp <- multicomp(energy.aov.4, focus="Stove",
                              adjust=list(Wood=c(
                                            "Osage Orange",
                                            "Red Oak",
                                            "Black Walnut",
                                            "White Pine")),
                              plot=FALSE,
                              method="tukey", valid.check=FALSE)
energy.multicomp$crit.point
##    tukey 
## 2.937812


## Cheung and Chan method
energy.nsize <- tapply(energy$Energy, energy[,c("Wood","Stove")], length)
energy.nsize
date()
energy.tpmc <-
  try(
  tpmc(NGROUP=4,
       NK=3,
       DF=energy.aov.4$df.residual,
       NSIZE=energy.nsize)
      )
if (class(energy.tpmc)==if.R(s="Error", r="try-error"))
  energy.tpmc <- 2.925457  ## value calculated on machine where tpmc is compiled
date()
##
##
energy.multicomp <- multicomp(energy.aov.4, focus="Stove",
                              adjust=list(Wood=c(
                                            "Osage Orange",
                                            "Red Oak",
                                            "Black Walnut",
                                            "White Pine")),
                              crit.point=energy.tpmc,
                              method="tpmc",
                              plot=FALSE)
energy.multicomp$crit.point
## [1] 2.925457
old.par <- par(oma=c(0,3,0,0))
plot(energy.multicomp, col.signif=8, lty.signif=1)
par(old.par)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.multicomp-original.eps"))
energy.multicomp$method <- "tpmc"
energy.multicomp <- multicomp.label.change(energy.multicomp, ".adj1", ".OsgOr")
energy.multicomp <- multicomp.label.change(energy.multicomp, ".adj2", ".RdOak")
energy.multicomp <- multicomp.label.change(energy.multicomp, ".adj3", ".BkWal")
energy.multicomp <- multicomp.label.change(energy.multicomp, ".adj4", ".WPine")
energy.multicomp <- multicomp.reverse(energy.multicomp)
old.par <- par(oma=c(0,3,0,0))
plot(energy.multicomp, col.signif=8, lty.signif=1)
par(old.par)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.multicomp.eps"))
dimnames(energy.multicomp$lmat)[[1]]

energy.multicomp.order <- energy.multicomp
energy.multicomp.order <-
  multicomp.order(energy.multicomp.order,
                  sort.order=c(2,1,3, 5,6,4, 9,8,7, 10,12,11))
old.par <- par(oma=c(0,3,0,0))
plot(energy.multicomp.order, col.signif=8, lty.signif=1)
par(old.par)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.multicomp.order.eps"))


energy.new <-
  data.frame(Moist=14,
             Wood=rep(levels(energy$Wood),c(3,3,3,3)),
             Stove=rep(levels(energy$Stove),4))
energy.new$Energy.adj <-
  predict(energy.aov.4, newdata=energy.new)
energy.new

energy.am.mmc <- list()
for (i in levels(energy.new$Wood))
  energy.am.mmc[[i]] <-
  multicomp.mmc.mean(energy.new$Stove[1:3],
                     energy.nsize[i,],
                     energy.new[energy.new$Wood==i,"Energy.adj"],
                     sqrt(anova(energy.aov.4)[6,"Mean Sq"]),
                     ylabel="adjusted Energy",
                     focus="Stove",
                     plot=FALSE,
                     method="tpmc",
                     crit.point=energy.tpmc)

trellis.device.hh.color(width=11, height=2.75)
old.par <- par(mfrow=c(1,4), mar=c(5,4,4,4)+.1)
for (i in levels(energy$Wood))
  plot(energy.am.mmc[[i]], ry=c(1.3,7.2), main=i, main2="", col.iso=16)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am.mmc.eps"))
par(old.par)

old.par <- par(mfrow=c(1,4), mar=c(5,4,4,4)+.1)
## MMC Figure 9a
for (i in levels(energy$Wood))
  plot(energy.am.mmc[[i]], ry=c(1.3,7.2), main=i, main2="", col.iso=16,
       xlab="", ylab="", focus="")
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am.mmc.n.eps"))
par(old.par)
## dev.off()

trellis.device.hh.color(width=11, height=2.5)
old.par <- par(mfrow=c(1,4), mar=c(5,4,4,4)+.1)
for (i in levels(energy$Wood))
  plot(energy.am.mmc[[i]],  main=i, main2="", col.iso=16)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am4.mmc.eps"))
par(old.par)
## dev.off()

## back to the standard-size graphics device
old.par <- par(mfrow=c(2,2), mar=c(5,4,4,6)+.1)
for (i in levels(energy$Wood))
  plot(energy.am.mmc[[i]],  main=i, main2="", col.iso=16)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am4-2x2.mmc.eps"))
par(old.par)

old.par <- par(mfrow=c(2,2), mar=c(5,4,4,6)+.1)
## MMC Figure 9b
for (i in levels(energy$Wood))
  plot(energy.am.mmc[[i]],  main=i, main2="", col.iso=16,
       xlab="", ylab="", focus="")
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am4-2x2.mmc.n.eps"))
par(old.par)

energy.am.mca <- energy.am.mmc[[levels(energy$Wood)[1]]]$mca
for (i in levels(energy$Wood)[-1])
  energy.am.mca$table <- rbind(energy.am.mca$table, energy.am.mmc[[i]]$mca$table)

## MMC Figure 9c
old.par <- par(mar=c(5,10,4,2)+.1)
plot(energy.am.mca, col.signif=8, lty.signif=1)
segments(-1.6, x2=2.8, y1=12.5, y2=12.5, lty=2, xpd=T)
segments(-1.6, x2=2.8, y1=15.5, y2=15.5, lty=2, xpd=T)
segments(-1.6, x2=2.8, y1=18.5, y2=18.5, lty=2, xpd=T)
axis(levels(energy$Wood), side=2, las=1, line=6,
     at=c(21.5, 18.5, 15.5, 12.5)-1.5,
     ticks=F)
## export.eps(h2("mmc/figure/AMDIII.ex.5.5.am.mca.eps"))
par(old.par)

energy.aov.4a <- aov(Energy ~ Moist*(Wood/Stove),
                    data=energy)
anova(energy.aov.4a)

trellis.par.set("superpose.line", tpg.sl.original)
trellis.par.set("superpose.symbol", tpg.ss.original)
