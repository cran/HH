### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/regbb.tex'

###################################################
### code chunk number 1: regbb.tex:167-174
###################################################
hhcapture("htwt-stem.Rout", '
data(htwt)
levels(factor(htwt$sex, exclude=NULL))
any(is.na(htwt$ht))
for (h in tapply(htwt$ht, factor(htwt$sex, exclude=NULL), c))
  stem(h, scale=1.5)
')


###################################################
### code chunk number 2: regbb.tex:202-212
###################################################
hhpdf("htwt-splom.pdf", width=5.5, height=5.5)
## assign values to the missing observations
match(NA, htwt$ht)
match(NA, htwt$sex)
htwt[c(4, 27),]
htwt[4,"ht"] <- round(1.65 * 39.37)  ## this student answered in meters
htwt[27,"sex"] <- "m" ## based on class list
splom(~ htwt[,c("lbs","months","sex","ht")],
      axis.text.cex=.7, xlab=NULL, pch=19)
hhdev.off()


###################################################
### code chunk number 3: regbb.tex:224-230
###################################################
hhpdf("htwt-xy.pdf", width=5, height=4)
xyplot(lbs ~ ht, data=htwt,
       groups=factor(sex, levels=c("m","f"), labels=c("male","female")),
       aspect=1, par.settings=list(superpose.symbol=list(pch=c(17,19))),
       auto.key=list(space="right", border=TRUE))
hhdev.off()


###################################################
### code chunk number 4: regbb.tex:251-257
###################################################
hhcapture("htwt-aov.Rout", '
## one-way analysis of variance
htwt.aov <- aov(ht ~ sex, data=htwt)
summary(htwt.aov)
model.tables(htwt.aov, type="means")
')


###################################################
### code chunk number 5: regbb.tex:283-290
###################################################
hhcapture("htwt-lm.Rout", '
## dummy variable
htwt$female <- as.numeric(htwt$sex == "f")
htwt.lm <- lm(ht ~ female, data=htwt)
summary(htwt.lm, corr=FALSE)
anova(htwt.lm)
')


###################################################
### code chunk number 6: regbb.tex:318-325
###################################################
hhcapture("htwtb-lm.Rout", '
## dummy variable
htwt$treat <- (htwt$sex == "f") - (htwt$sex == "m")
htwtb.lm <- lm(ht ~ treat, data=htwt)
summary(htwtb.lm, corr=FALSE)
anova(htwtb.lm)
')


###################################################
### code chunk number 7: regbb.tex:406-449
###################################################
W.simple <- cbind(Int=1, contr.treatment(4, contrasts=FALSE))
W.simple

W.treatment <- cbind(Int=1, contr.treatment(4))
W.treatment

W.helmert <- cbind(Int=1, contr.helmert(4))
dimnames(W.helmert)[[2]][2:4] <- 2:4
W.helmert

W.sum <- cbind(Int=1, contr.sum(4))
dimnames(W.sum)[[2]][2:4] <- 2:4
W.sum

W.poly <- cbind(Int=1, contr.poly(4))
row.names(W.poly) <- 1:4
W.poly


A.treatment <- cbind("1"=c(1,0,0,0,0),
                     "2"=c(0,0,1,0,0),
                     "3"=c(0,0,0,1,0),
                     "4"=c(0,0,0,0,1))
A.treatment
W.simple %*% A.treatment

A.helmert <- cbind("1"=c(1, 0,0,  0, 0),
                   "2"=c(1,-2, 0,-1,-1),
                   "3"=c(1,-2,-2, 1,-1),
                   "4"=c(1,-2,-2,-2, 2))
A.helmert
W.simple %*% A.helmert

A.sum <- cbind("1"=c(1,0,0,0, 0),
               "2"=c(0,1,0,0,-1),
               "3"=c(0,0,1,0,-1),
               "4"=c(0,0,0,1,-1))
A.sum
W.simple %*% A.sum

A.poly <- MASS::ginv(W.simple) %*% W.poly
zapsmall(A.poly)
W.simple %*% A.poly


###################################################
### code chunk number 8: regbb.tex:719-726
###################################################
hhpdf("fabricwear.pdf", width=7, height=3)
data(fabricwear)
bwplot(wear ~ speed, data=fabricwear,
       scales=list(cex=1.4), xlab=list(cex=1.4), ylab=list(cex=1.4),
       par.settings=list(box.dot=list(
          col=trellis.par.get()$superpose.symbol$col[1])))
hhdev.off()


###################################################
### code chunk number 9: regbb.tex:739-744
###################################################
hhcapture("fabricwear1.Rout", '
fabricwear.aov <- aov(wear ~ speed, data=fabricwear)
summary(fabricwear.aov)
model.tables(fabricwear.aov, "mean")
')


###################################################
### code chunk number 10: regbb.tex:785-797
###################################################
hhcapture("fabricwear2.Rout", '
tmp.c <- zapsmall(contrasts(fabricwear$speed), 14)
dimnames(tmp.c)[[1]] <- levels(fabricwear$speed)
tmp.c
zapsmall(crossprod(tmp.c), 13)
min.nonzero <- function(x, digits=13) {
  xx <- zapsmall(x, digits)
  min(xx[xx != 0])
}
tmp.min <- apply(abs(tmp.c), 2, min.nonzero)
sweep(tmp.c, 2, tmp.min, "/")
')


###################################################
### code chunk number 11: regbb.tex:817-833
###################################################
hhpdf("orthpoly.pdf", width=9, height=4.4)
tmp.tr <- data.frame(polynomial=as.vector(tmp.c),
                     speed=rep(as.numeric(dimnames(tmp.c)[[1]]),5),
                     power=rep(ordered(dimnames(tmp.c)[[2]],
                       levels=dimnames(tmp.c)[[2]]), c(6,6,6,6,6)))

xyplot(polynomial ~ speed | power, data= tmp.tr, type="b",
       layout=c(5,1), between=list(x=1, y=1),
       ylab="normalized orthogonal polynomials",
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0, lty=2, col="gray40")
       },
       scales=list(cex=1, alternating=1),
       par.strip.text=list(cex=1.4))
hhdev.off()


###################################################
### code chunk number 12: regbb.tex:863-869
###################################################
hhcapture("fabricwear3.Rout", '
summary(fabricwear.aov,
        split=list(speed=list(speed.L=1, speed.Q=2,
                   speed.C=3, rest=4:5)))
summary.lm(fabricwear.aov)
')


###################################################
### code chunk number 13: regbb.tex:1100-1104
###################################################
hhpdf("hotdog1.pdf", height=3, width=5)
data(hotdog)
bwplot(Sodium ~ Type, data=hotdog, panel=panel.bwplot.superpose, groups=Type)
hhdev.off()


###################################################
### code chunk number 14: regbb.tex:1134-1146
###################################################
hhpdf("hotdog-f0.pdf", height=3.5, width=8)
hotdog.key <- list(title="Type", border=TRUE, space="right",
                   text=list(levels(hotdog$Type)),
                   points=list(pch=15:17),
                   lines=list(lty=1),
                   col=trellis.par.get("superpose.symbol")$col[1:3])
TxC <- ancovaplot(Sodium ~ Type, x=Calories, data=hotdog,
                  main="Sodium ~ Type, x=Calories",
                  scales=list(alternating=FALSE),
                  between=list(x=c(0,0,1)))
update(TxC, key=hotdog.key)
hhdev.off()


###################################################
### code chunk number 15: regbb.tex:1170-1176
###################################################
hhcapture("hotdog-anova1.Rout", '
## aovStatementAndAnova(TxC)
TxC.aov <- aov(Sodium ~ Type, data=hotdog)
anova(TxC.aov)
model.tables(TxC.aov, type="means")
')


###################################################
### code chunk number 16: regbb.tex:1225-1232
###################################################
hhpdf("hotdog-f3.pdf", height=3.5, width=8)
CgT <- ancovaplot(Sodium ~ Calories, groups=Type, data=hotdog,
                  main="Sodium ~ Calories, groups=Type",
                  scales=list(alternating=FALSE),
                  between=list(x=c(0,0,1)))
update(CgT, key=hotdog.key)
hhdev.off()


###################################################
### code chunk number 17: regbb.tex:1257-1262
###################################################
hhcapture("hotdog-ancova-f3.Rout", '
## aovStatementAndAnova(CgT, warn=FALSE)
CgT.aov <- aov(Sodium ~ Calories, data=hotdog)
anova(CgT.aov)
')


###################################################
### code chunk number 18: regbb.tex:1278-1285
###################################################
hhpdf("hotdog-f1.pdf", height=3.5, width=8)
CpT <- ancovaplot(Sodium ~ Calories + Type, data=hotdog,
                  main="Sodium ~ Calories + Type",
                  scales=list(alternating=FALSE),
                  between=list(x=c(0,0,1)))
update(CpT, key=hotdog.key)
hhdev.off()


###################################################
### code chunk number 19: regbb.tex:1304-1309
###################################################
hhcapture("hotdog-ancova2.Rout", '
## aovStatementAndAnova(CpT)
CpT.aov <- aov(Sodium ~ Calories + Type, data=hotdog)
anova(CpT.aov)
')


###################################################
### code chunk number 20: regbb.tex:1346-1355
###################################################
hhpdf("hotdog-f4.pdf", height=3.5, width=8)
hotdog$Sodium.Calories <-
   hotdog$Sodium - predict.lm(CpT.aov, type="terms", terms="Calories") ## aov is NOT generic
T.C <- ancovaplot(Sodium.Calories ~ Type, x=Calories, data=hotdog,
                  main="Sodium.Calories ~ Type, x=Calories",
                  scales=list(alternating=FALSE),
                  between=list(x=c(0,0,1)))
update(T.C, key=hotdog.key)
hhdev.off()


###################################################
### code chunk number 21: regbb.tex:1391-1396
###################################################
hhcapture("hotdog-ancovaf4.Rout", '
## aovStatementAndAnova(T.C)
T.C.aov <- aov(Sodium.Calories ~ Type, data=hotdog)
anova(T.C.aov)
')


###################################################
### code chunk number 22: regbb.tex:1419-1423
###################################################
hhcapture("hotdog-ancova2b.Rout", '
CpT.mmc <- mmc(CpT.aov)
CpT.mmc
')


###################################################
### code chunk number 23: regbb.tex:1440-1443
###################################################
hhpdf("hotdog3.pdf", height=6, width=7.5)
mmcplot(CpT.mmc)
hhdev.off()


###################################################
### code chunk number 24: regbb.tex:1478-1485
###################################################
hhpdf("hotdog-f2.pdf", height=3.5, width=8)
CsT <- ancovaplot(Sodium ~ Calories * Type, data=hotdog,
                  main="Sodium ~ Calories * Type",
                  scales=list(alternating=FALSE),
                  between=list(x=c(0,0,1)))
update(CsT, key=hotdog.key)
hhdev.off()


###################################################
### code chunk number 25: regbb.tex:1503-1508
###################################################
hhcapture("hotdog-ancova3.Rout", '
## aovStatementAndAnova(CsT)
CsT.aov <- aov(Sodium ~ Calories * Type, data=hotdog)
anova(CsT.aov)
')


###################################################
### code chunk number 26: regbb.tex:1550-1587
###################################################
hhpdf("ancova-composite.pdf", height=7, width=9)
removeAnnotation <-
       function(x) {
         update(x,
                main=list(x$main, cex=1.1),
                ylab=NULL,
                xlab=NULL,
                legend=NULL,
                scales=list(alternating=0, tck=0),
                par.strip.text=list(cex=.9, lines=1.1))
      }

## 2 x 3, with empty spots
print(position=c(.03, .31,  .53, .62), more=TRUE, removeAnnotation(CgT))
print(position=c(.50, .00, 1.00, .31), more=TRUE, removeAnnotation(TxC))
print(position=c(.50, .31, 1.00, .62), more=TRUE, removeAnnotation(CpT))
print(position=c(.50, .62, 1.00, .93), more=TRUE, removeAnnotation(CsT))

## column labeling
grid.text(x=c(.29, .75), y=.02, gp=gpar(fontsize=14),
          c(expression("constant intercept" ~~ alpha),
            expression("variable intercept" ~~ alpha)))

## row labeling
grid.text(x=.02, y=c(.15, .45, .75), rot=90, gp=gpar(fontsize=14),
          c(expression("zero slope" ~~ beta==0),
            expression("constant slope" ~~ beta),
            expression("variable slope" ~~ beta)))

## main title
grid.text(x=.5, y=.98, gp=gpar(fontsize=18),
          "Composite graph illustrating four models with a factor and a covariate")
lattice:::lattice.setStatus(print.more = FALSE)
hhdev.off()
## hhpdf("demoancova.pdf", height=7, width=9)
## demo(ancova, ask=FALSE)
## hhdev.off()


###################################################
### code chunk number 27: regbb.tex:1642-1661
###################################################
hhcode("hotdog-ancova.r", '
data(hotdog, package="HH")

## constant line across all groups
## y ~ x
ancovaplot(Sodium ~ Calories, groups=Type, data=hotdog)

## different horizontal line in each group
## y ~ a
ancovaplot(Sodium ~ Type, x=Calories, data=hotdog)

## constant slope, different intercepts
## y ~ x + a  or  y ~ a + x
ancovaplot(Sodium ~ Calories + Type, data=hotdog)

## different slopes, and different intercepts
## y ~ x * a  or  y ~ a * x
ancovaplot(Sodium ~ Calories * Type, data=hotdog)
')


