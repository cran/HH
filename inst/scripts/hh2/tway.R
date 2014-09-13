### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/tway.tex'

###################################################
### code chunk number 1: tway.tex:79-85
###################################################
hhpdf("display.pdf")
data(display)
interaction2wt(time ~ panel.ordered + emergenc, data=display,
               par.strip.text=list(cex=.8),
               key.cex.title=.8)
hhdev.off()


###################################################
### code chunk number 2: tway.tex:113-117
###################################################
hhcapture("display2.Rout", '
displayf.aov <- aov(time ~ emergenc * panel, data=display)
anova(displayf.aov)
')


###################################################
### code chunk number 3: tway.tex:148-152
###################################################
hhcapture("display2a.Rout", '
displayf.mmc <- mmc(displayf.aov, focus="panel")
displayf.mmc
')


###################################################
### code chunk number 4: tway.tex:189-194
###################################################
hhcapture("display2b.Rout", '
displayr.aov <- aov(time ~ Error(emergenc/panel) + panel,
                    data=display)
summary(displayr.aov)
')


###################################################
### code chunk number 5: tway.tex:216-219
###################################################
hhpdf("displaymmc.pdf")
mmcplot(displayf.mmc, style="both")
hhdev.off()


###################################################
### code chunk number 6: tway.tex:250-259
###################################################
hhpdf("displaymc.pdf", width=7, height=2.3)
mmcplot(displayf.mmc, type="none",
        xlab="time",
        ylab="mean time",
        ylab.right="panel level",
        xlim=c(15, 29),
        axis.right=1.1,
        contrast.label=FALSE)
hhdev.off()


###################################################
### code chunk number 7: tway.tex:829-843
###################################################
hhpdf("plasmaint.pdf")
data(plasma)
plasma$id <-
   factor(plasma$id,
          levels=order(with(plasma, tapply(plasma, id, median))))
interaction2wt(plasma ~ time + id, data=plasma)
hhdev.off()

## several additional views of the effect of the time factor
## conditioned on patients
interaction2wt(plasma ~ time + id, data=plasma, simple=TRUE, main="simple effects")
xyplot(plasma ~ time | id, data=plasma, type="b", pch=19, layout=c(10, 1), between=list(x=.5))
xyplot(plasma ~ time | id, data=plasma, type="b", pch=19, layout=c(1, 10),
       strip=FALSE, strip.left=TRUE)


###################################################
### code chunk number 8: tway.tex:863-867
###################################################
hhcapture("plasma.Rout", '
plasma.aov <- aov(plasma ~ Error(id) + time, data=plasma)
summary(plasma.aov)
')


###################################################
### code chunk number 9: tway.tex:891-897
###################################################
hhpdf("plasmammc.pdf")
plasma$time <-
   factor(plasma$time, levels=unique(plasma$time), ordered=FALSE)
plasma.aov <- aov(plasma ~ id + time, data=plasma)
mmcplot(mmc(plasma.aov, focus="time"), h=c(.6, .4), style="both")
hhdev.off()


###################################################
### code chunk number 10: tway.tex:1047-1059
###################################################
hhpdf("workstation.pdf", height=3.5)
data(workstation)
bwplot(devices ~ station | method, data=workstation,
       ylab=list(cex=1.4),
       xlab=list("station %in% method", cex=1.4),
       strip=strip.custom(strip.names=c(TRUE, TRUE)),
       par.strip.text=list(cex=1.4),
       scales=list(x=list(cex=1),y=list(cex=1.2)),
       layout=c(3,1),
       par.settings=list(box.dot=list(
          col=trellis.par.get()$superpose.symbol$col[1])))
hhdev.off()


###################################################
### code chunk number 11: tway.tex:1130-1136
###################################################
hhcapture("workstation.Rout", '
workstation.aov <- aov(devices ~ method / station,
                       data=workstation)
summary(workstation.aov)
model.tables(workstation.aov, "means", se=TRUE)
')


###################################################
### code chunk number 12: tway.tex:1393-1407
###################################################
hhcapture("StudentizedRange.Rout", '
## This output table is not included as a Table in the book.
## The numbers calculated here are incorporated into the text of the section
## on "Studentized Range Distribution".
summary(displayf.aov)
displayf.mmc
qtukey(.95, 3, 12)/sqrt(2)     ## The Studentized Range Distribution
attr(confint(displayf.mmc$mca$glht)$confint, "calpha")  ## R: Estimated Quantile
attr(confint(displayf.mmc$mca$glht)$confint, "calpha") * sqrt(2)  ## SAS: Critical Value of Studentized Range

ms.res <- summary(displayf.aov)[[1]]["Residuals","Mean Sq"]
ms.res
sqrt(2*ms.res/8) * attr(confint(displayf.mmc$mca$glht)$confint, "calpha")  ## minimum significant difference
')


###################################################
### code chunk number 13: tway.tex:1499-1516
###################################################
data(rhiz.alfalfa)
alfalfa <- reshape2::melt(rhiz.alfalfa, id=c("comb","strain"))
hhpdf("alfalfa.pdf")
useOuterStrips(combineLimits(
  bwplot(value ~ strain | comb * variable, data=alfalfa,
         main="Alfalfa Experiment\n", layout=c(2, 3),
         par.strip.text=list(cex=.9),
         between=list(x=1, y=1),
         ylab=NULL, xlab="strain", xlab.top="Combination",
         scales=list(
           cex=.75,
           y=list(relation="free")),
         as.table=TRUE,
         par.settings=list(box.dot=list(
            col=trellis.par.get()$superpose.symbol$col[1])))
  ))
hhdev.off()


###################################################
### code chunk number 14: tway.tex:1525-1542
###################################################
data(rhiz.clover)
clover <- reshape2::melt(rhiz.clover, id=c("comb","strain"))
hhpdf("clover.pdf")
useOuterStrips(combineLimits(
  bwplot(value ~ strain | comb * variable, data=clover,
         main="Clover Experiment\n", layout=c(2, 3),
         par.strip.text=list(cex=.9),
         between=list(x=1, y=1),
         ylab=NULL, xlab="strain", xlab.top="Combination",
         scales=list(
           cex=.75,
           y=list(relation="free")),
         as.table=TRUE,
         par.settings=list(box.dot=list(
            col=trellis.par.get()$superpose.symbol$col[1])))
  ))
hhdev.off()


###################################################
### code chunk number 15: tway.tex:1582-1624
###################################################
useOuterStrips(combineLimits(
  bwplot(value ~ comb | strain * variable, data=clover,
         main="Clover Experiment\n", layout=c(6, 3),
         par.strip.text=list(cex=.9),
         between=list(x=1, y=1),
         ylab=NULL, xlab="comb", xlab.top="strain",
         scales=list(
           cex=.75,
           y=list(relation="free")),
         as.table=TRUE,
         par.settings=list(box.dot=list(
            col=trellis.par.get()$superpose.symbol$col[1])))
  ))

useOuterStrips(combineLimits(
  bwplot(strain ~ value | variable * comb, data=clover,
         main="Clover Experiment", layout=c(3, 2),
         par.strip.text=list(cex=.9),
         between=list(x=1, y=1),
         xlab=NULL, ylab="strain",
         scales=list(
           cex=.75,
           x=list(relation="free")),
         as.table=TRUE,
         par.settings=list(box.dot=list(
            col=trellis.par.get()$superpose.symbol$col[1])))
  ))


useOuterStrips(combineLimits(
  bwplot(comb ~ value | variable * strain, data=clover,
         main="Clover Experiment", layout=c(3, 6),
         par.strip.text=list(cex=.9),
         between=list(x=1, y=1),
         xlab=NULL, ylab="Combination",
         scales=list(
           cex=.75,
           x=list(relation="free")),
         as.table=TRUE,
         par.settings=list(box.dot=list(
            col=trellis.par.get()$superpose.symbol$col[1])))
  ))


###################################################
### code chunk number 16: tway.tex:1639-1649
###################################################
hhcapture("rhiz-alf-aov.Rout", '
## unset position(rhiz.alfalfa$comb) for glht
data(rhiz.alfalfa) ## fresh copy of the data.
rhiz.alfalfa.aov <- aov(Npg ~ strain * comb, data=rhiz.alfalfa)
summary(rhiz.alfalfa.aov)

alf.means <- model.tables(rhiz.alfalfa.aov, type="means",
                          se=TRUE, cterms="strain")
alf.means
')


###################################################
### code chunk number 17: tway.tex:1703-1728
###################################################
hhpdf("alfmeans.pdf", height=3.5)
old.fin <- par()$fin
par(fin=c(old.fin[1], 2.5))

plot(y=c(1.01,1,.99,1,1,1), x=alf.means$tables$strain,
     pch=16,
     xlim=c(29.5, 32.7),
     yaxt="n", ylim=c(.85,1.10), ylab="",
     xaxt="n", xlab="")
rug(alf.means$tables$strain, ticksize=-.1)
axis(3)
mtext("Npg", 3, line=3)
lines(x=c(29.7,31.5), y=c(.95,.95))
lines(x=c(30.5,32.5), y=c(.90,.90))
strain.labels <- paste(dimnames(alf.means$tables$strain)[[1]],
                       format(alf.means$tables$strain, digits=6),
                       sep=": ")

axis(1, at=alf.means$tables$strain[c(2,4, 6)], labels=strain.labels[c(2,4,  6)], line=1, tick=FALSE, adj=.45)
axis(1, at=alf.means$tables$strain[c(    5  )], labels=strain.labels[c(    5  )], line=2, tick=FALSE, adj=.45)
axis(1, at=alf.means$tables$strain[3         ], labels=strain.labels[3         ], line=2, tick=FALSE, adj=.45)
axis(1, at=alf.means$tables$strain[1         ], labels=strain.labels[1         ], line=3, tick=FALSE, adj=.4)

par(fin=old.fin)
hhdev.off()


###################################################
### code chunk number 18: tway.tex:1745-1749
###################################################
hhpdf("alfalfammc.pdf", height=8, width=8)
alf.mmc <- mmc(rhiz.alfalfa.aov, focus="strain")
mmcplot(alf.mmc, h=c(.45, .55), style="both")
hhdev.off()


###################################################
### code chunk number 19: tway.tex:1798-1817
###################################################
hhpdf("alfalfalmatmmc.pdf", height=8, width=8)
alf.comp <- cbind("1,7,10-c"=c(-3, 0, 0, 1, 1, 1),
                  "1,10-7"  =c( 0, 0, 0, 1, 1,-2),
                  "1-10"    =c( 0, 0, 0, 1,-1, 0),
                  "15-12"   =c( 0, 1,-1, 0, 0, 0),
            "1,7,10,c-12,15"=c( 1,-2,-2, 1, 1, 1))
dimnames(alf.comp)[[1]] <- dimnames(alf.mmc$none$lmat)[[2]]
alf.mmc <- mmc(rhiz.alfalfa.aov, focus="strain",
                         focus.lmat=alf.comp)
alf.mmc
alf.both <- mmcplot(alf.mmc, h=c(.45, .55), type="lmat", style="both")  ## include top panel in Figure
alf.both
hhdev.off()
hhpdf("alfalfalmatmmc2.pdf", height=8, width=8)
## This hack gets a smaller height with the same width for the Tiebreaker plot.
alf.both2 <- alf.both
alf.both2$par.settings$layout.heights$panel <- c(.75, .25)
alf.both2                                                 ## include bottom panel in Figure
hhdev.off()


###################################################
### code chunk number 20: tway.tex:1924-1945
###################################################
hhpdf("clovint2wt.pdf", height=8, width=8)
rcc <- rhiz.clover$comb ## save factor
position(rhiz.clover$comb) <- c(2, 5)
interaction2wt(Npg ~ strain + comb, data=rhiz.clover,
               ## scales=list(x=list(cex=.5), labels=list(rot=90)),
               par.settings=list(
                 plot.symbol=list(pch=19),
                 box.dot=list(pch=19),
                 axis.text=list(cex=.6)  ## replace this line
                 ))
hhdev.off()

## not shown in book:
interaction2wt(Npg ~ strain + comb, data=rhiz.clover,
               simple=TRUE, simple.scale=list(strain=.4, comb=.2),
               par.settings=list(
                 plot.symbol=list(pch=19),
                 box.dot=list(pch=19),
                 axis.text=list(cex=.6)  ## replace this line
                 ))
rhiz.clover$comb <- rcc ## restore to factor


###################################################
### code chunk number 21: tway.tex:1981-1987
###################################################
hhcapture("rhiz-clov-aov.Rout", '
rhiz.clover.aov <- aov(Npg ~ strain * comb, data=rhiz.clover)
summary(rhiz.clover.aov)

model.tables(rhiz.clover.aov, type="means", se=TRUE)
')


###################################################
### code chunk number 22: tway.tex:2040-2053
###################################################
hhcapture("rhiz-clov-nest-aov.Rout", '
rhiz.clover.nest.aov <-
    aov(Npg ~ comb/strain, data=rhiz.clover)
summary(rhiz.clover.nest.aov)

old.width <- options(width=35)
names(coef(rhiz.clover.nest.aov))
options(old.width)
summary(rhiz.clover.nest.aov,
        split=list("comb:strain"=
          list(clover=c(1,3,5,7,9),
               "clover+alf"=c(2,4,6,8,10))))
')


###################################################
### code chunk number 23: tway.tex:2077-2093
###################################################
hhcapture("rhiz-clov-nest-aov-x.Rout", '
## Look at the contrasts, their generated dummy variables,
## and their regression coefficients.
## Abbreviate their names for presentation.
tmp <- abbreviate(names(coef(rhiz.clover.nest.aov)))
## tmp

## contrasts(rhiz.clover$comb)
## contrasts(rhiz.clover$strain)

cnx <- aov(Npg ~ comb/strain, data=rhiz.clover, x=TRUE)$x
dimnames(cnx)[[2]] <- tmp
## cnx
cnx[seq(1,60,5), c(1,2,  3,5,7,9,11)]
cnx[seq(1,60,5), c(4,6,8,10,12)]
')


###################################################
### code chunk number 24: tway.tex:2115-2121
###################################################
hhcapture("rhiz-clov-nest-aov-x2.Rout", '
cnxb <- round(coef(summary.lm(rhiz.clover.nest.aov)), 3)
dimnames(cnxb)[[1]] <- tmp
## cnxb
cnxb[c(1,2,  3,5,7,9,11, 4,6,8,10,12),]
')


###################################################
### code chunk number 25: tway.tex:2139-2157
###################################################
## The next few code chunks are the setup for the three figures showing
## MMC plots of simple effects of the clover data.  All the clover simple
## effect MMC plots require height=9in and width=16in.

rhiz.clover$cs <- with(rhiz.clover, interaction(comb, strain))
levels(rhiz.clover$cs)
rhiz.clover.cs.aov <- aov(Npg ~ cs, data=rhiz.clover)
summary(rhiz.clover.cs.aov)

## This is the default with 12 groups in the pairwise comparisons of the levels of cs.
## It acts as if both the clover and clover+alfalfa means can be compared.
## It uses calpha appropriate for comparing 12 groups.
## This plot is so heavily overprinted that it is not in the book.
## We show refinements that are in the book.
cs12.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"))
mmcplot(cs12.mmc,
  main="clover and clover+alfalfa comparisons --- combn(12,2) == 66,\ncalpha=qtukey(.95, 12, 48)",
  sub="Very heavily overprinted.  Use the next few plots instead.")


###################################################
### code chunk number 26: tway.tex:2160-2173
###################################################
## This code chunk gets the common mmc object to be used in the next three figures.
## It uses the calpha appropriate for 6 groups, either the clover or the clover+alfalfa,
## but not both.  It calculates the xlim and ylim for the next three figures.
## This plot is so heavily overprinted that it is not in the book.
## We show subsets of this plot in the book.
cs.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
              calpha=qtukey( .95, 6,  48)/sqrt(2))
cs.mmcplot <- mmcplot(cs.mmc,
   main="clover and clover+alfalfa comparisons --- combn(12, 2) == 66,\nbut qtukey(.95, 6, 48)",
  sub="Very heavily overprinted.  Use the next few plots instead.")
cs.mmcplot
csc.xlim <- cs.mmcplot$x.limits
csc.ylim <- cs.mmcplot$y.limits


###################################################
### code chunk number 27: tway.tex:2177-2208
###################################################
## This code chunk constructs the lmat matrices for just the clover contrasts
## and for just the clover+alfalfa contrasts.
dlmat2 <- dimnames(cs.mmc$mca$lmat)[[2]]

cl.index <- grep("clover\\.[[:print:]]*clover\\.", dlmat2, value=TRUE)
cl.index
clal.index <- grep("clover\\+[[:print:]]*clover\\+", dlmat2, value=TRUE)
clal.index


clover.lmat <- cs.mmc$mca$lmat[, cl.index]
dimnames(clover.lmat)[[1]]
dimnames(clover.lmat)[[1]] <- levels(rhiz.clover$cs)
clover.lmat[1,] <- -colSums(clover.lmat[-1, ])
clover.lmat

cloveralf.lmat <- cs.mmc$mca$lmat[, clal.index]
dimnames(cloveralf.lmat)[[1]]
dimnames(cloveralf.lmat)[[1]] <- levels(rhiz.clover$cs)
cloveralf.lmat[1,] <- -colSums(cloveralf.lmat[-1, ])
cloveralf.lmat

cloverorth.lmat <- cbind(
##                      c.5   c.1 c+a.1 c+a.5  c.kc c+a.7 c+a.kc c+a.13 c+a.4   c.7  c.13   c.4
"clover.5-rest"    =c(    5,   -1,    0,    0,   -1,    0,     0,     0,    0,   -1,   -1,   -1),
"clover.1-4.7.13"  =c(    0,    3,    0,    0,    0,    0,     0,     0,    0,   -1,   -1,   -1),
"clover.c-1.4.7.13"=c(    0,   -1,    0,    0,    4,    0,     0,     0,    0,   -1,   -1,   -1),
"clover.7-13"      =c(    0,    0,    0,    0,    0,    0,     0,     0,    0,    1,   -1,    0),
"clover.7.13-4"    =c(    0,    0,    0,    0,    0,    0,     0,     0,    0,    1,    1,   -2))
dimnames(cloverorth.lmat)[[1]] <- dimnames(cs.mmc$none$lmat)[[2]]
cloverorth.lmat


###################################################
### code chunk number 28: tway.tex:2213-2229
###################################################
## clover with suppression of clover+alfalfa ticks
## mmcplot is not in the book.  mmcboth is in the book.
csc.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
               focus.lmat=clover.lmat,
               calpha=qtukey( .95, 6,  48)/sqrt(2))

mmcplot(mmcPruneIsomeans(csc.mmc, keep = c(1,2,5,10,11,12)),
        xlim=csc.xlim, ylim=csc.ylim,
        type="lmat", main="clover comparisons --- combn(6,2) == 15")

hhpdf("cloverstrclovmmc.pdf", height=9, width=16)
mmcplot(mmcPruneIsomeans(csc.mmc, keep = c(1,2,5,10,11,12)),
        xlim=csc.xlim+c(-12, 3), ylim=csc.ylim, h=c(.6, .4),
        type="lmat", style="both",
        main="clover comparisons --- combn(6,2) == 15")
hhdev.off()


###################################################
### code chunk number 29: tway.tex:2256-2272
###################################################
## orthogonal contrasts for clover with suppression of clover+alfalfa ticks
## mmcplot is not in the book.  mmcboth is in the book.
csco.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
                focus.lmat=cloverorth.lmat,
                calpha=qtukey( .95, 6,  48)/sqrt(2))

mmcplot(mmcPruneIsomeans(csco.mmc, keep = c(1,2,5,10,11,12)),
        xlim=csc.xlim, ylim=csc.ylim,
        type="lmat", main="clover orthogonal contrasts --- 6 groups --> 5 contrasts")

hhpdf("cloverstrclovlmatmmc.pdf", height=9, width=16)
mmcplot(mmcPruneIsomeans(csco.mmc, keep = c(1,2,5,10,11,12)),
        xlim=csc.xlim+c(-12, 3), ylim=csc.ylim, h=c(.6, .4),
        type="lmat", style="both",
        main="clover orthogonal contrasts --- 6 groups --> 5 contrasts")
hhdev.off()


###################################################
### code chunk number 30: tway.tex:2299-2318
###################################################
## clover+alfalfa with suppression of clover ticks
## mmcplot is not in the book.  mmcboth is in the book.

csca.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
                focus.lmat=cloveralf.lmat,
                calpha=qtukey( .95, 6,  48)/sqrt(2))
row.names(csca.mmc$lmat$table) <-
  gsub("clover+alfalfa", "cl+alf", row.names(csca.mmc$lmat$table), fixed=TRUE)

mmcplot(mmcPruneIsomeans(csca.mmc, keep = c(3,4,6,7,8,9)),
        xlim=csc.xlim, ylim=csc.ylim,
        type="lmat", main="clover+alfalfa comparisons --- combn(6,2) == 15")

hhpdf("cloverstrclovalfmmc.pdf", height=9, width=16)
mmcplot(mmcPruneIsomeans(csca.mmc, keep = c(3,4,6,7,8,9)),
        xlim=csc.xlim+c(-12, 3), ylim=csc.ylim, h=c(.6, .4),
        type="lmat", style="both",
        main="clover+alfalfa comparisons --- combn(6,2) == 15")
hhdev.off()


###################################################
### code chunk number 31: tway.tex:2441-2446
###################################################
hhpdf("feed-i2wt.pdf", height=6, width=8)
data(feed)
interaction2wt(retained ~ supp + temp, data=feed,
               main.cex=1.6, scales=list(cex=.9))
hhdev.off()


###################################################
### code chunk number 32: tway.tex:2461-2465
###################################################
hhcapture("feed2.Rout", '
feed.int.aov <- aov(retained ~ temp * supp, data=feed)
anova(feed.int.aov)
')


###################################################
### code chunk number 33: tway.tex:2490-2498
###################################################
hhcapture("feed3.Rout", '
feed.aov <- aov(retained ~ temp + supp, data=feed)
anova(feed.aov)
summary(feed.aov, split=
        list(temp=list(linear=1, quadratic=2),
             supp=list(linear=1, quadratic=2, rest=3:4)))
model.tables(feed.aov, type="means", se=TRUE)
')


###################################################
### code chunk number 34: tway.tex:2548-2556
###################################################
hhpdf("feedsuppMMC.pdf", height=6, width=8)
data(feed)
feed$temp <- factor(feed$temp, ordered=FALSE)
feed$supp <- factor(feed$supp, ordered=FALSE)
feed.aov <- aov(retained ~ temp + supp, data=feed)
mmcplot(mmc(feed.aov, focus="supp"), h=c(.6, .4), style="both")
hhdev.off()
mmcplot(mmc(feed.aov, focus="temp"), style="both") ## not in book


###################################################
### code chunk number 35: tway.tex:2569-2574
###################################################
hhpdf("feedsuppMMCorth.pdf", height=6, width=8)
supp.poly <- contr.poly(5)
row.names(supp.poly) <- levels(feed$supp)
mmcplot(mmc(feed.aov, focus="supp", focus.lmat=supp.poly), type="lmat", h=c(.6, .4), style="both")
hhdev.off()


