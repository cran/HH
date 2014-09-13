### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/oway.tex'

###################################################
### code chunk number 1: oway.tex:58-65
###################################################
hhpdf("catalystm1.pdf", width=5, height=3)
data(catalystm)
bwplot(concent ~ catalyst, data=catalystm,
       panel=panel.bwplot.superpose, groups=catalyst,
       ylab=list("concentration"),
       xlab=list("catalyst"))
hhdev.off()


###################################################
### code chunk number 2: oway.tex:94-99
###################################################
hhcapture("catalystm-aov1.Rout", '
catalystm1.aov <- aov(concent ~ catalyst, data=catalystm)
anova(catalystm1.aov)
model.tables(catalystm1.aov, "means")
')


###################################################
### code chunk number 3: oway.tex:463-468
###################################################
hhcapture("catalystm-aov2.Rout", '
catalystm.mmc <-
   mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))
catalystm.mmc
')


###################################################
### code chunk number 4: oway.tex:509-512
###################################################
hhpdf("catalystm-mmc-mmc.pdf", width=6, height=6)
mmcplot(catalystm.mmc, style="both")
hhdev.off()


###################################################
### code chunk number 5: oway.tex:700-705
###################################################
hhcapture("batch.Rout", '
data(batch)
batch1.aov <- aov(Calcium ~ Batch, data=batch)
anova(batch1.aov)
')


###################################################
### code chunk number 6: oway.tex:722-725
###################################################
hhcapture("batchhov.Rout", '
hovBF(Calcium ~ Batch, data=batch)
')


###################################################
### code chunk number 7: oway.tex:740-743
###################################################
hhpdf("batchhov.pdf", width=7, height=4)
hovplotBF(Calcium ~ Batch, data=batch)
hhdev.off()


###################################################
### code chunk number 8: oway.tex:760-763
###################################################
hhpdf("batchoway.pdf", width=5, height=4)
OneWayVarPlot(Calcium ~ Batch, data = batch)
hhdev.off()


###################################################
### code chunk number 9: oway.tex:888-893
###################################################
hhpdf("turkey-f1.pdf", width=6, height=4)
data(turkey)
bwplot(wt.gain ~ diet, data=turkey, groups=diet,
       panel=panel.bwplot.superpose, xlab="diet", ylab="Weight Gain")
hhdev.off()


###################################################
### code chunk number 10: oway.tex:904-909
###################################################
hhcapture("turkey-aov1.Rout", '
turkey.aov <- aov(wt.gain ~ diet, data=turkey)
summary(turkey.aov)
model.tables(turkey.aov, type="means", se=TRUE)
')


###################################################
### code chunk number 11: oway.tex:947-958
###################################################
hhcapture("turkey-contrasts.Rout", '
contrasts(turkey$diet)
contrasts(turkey$diet) <-
  cbind(control.vs.treatment=c(1,-.25,-.25,-.25,-.25),
        A.vs.B              =c(0, .5,  .5, -.5, -.5 ),
        amount              =c(0, .5, -.5,  .5, -.5 ),
        A.vs.B.by.amount    =c(0, .5, -.5, -.5,  .5 ))
contrasts(turkey$diet)
tapply(turkey$wt.gain, turkey$diet, mean) %*%
   contrasts(turkey$diet)
')


###################################################
### code chunk number 12: oway.tex:976-988
###################################################
hhcapture("turkey-anova-contrasts.Rout", '
turkey2.aov <- aov(wt.gain ~ diet, data=turkey)
summary(turkey2.aov)
old.width <- options(width=67)
summary(turkey2.aov,
        split=list(diet=list(
                     control.vs.treatment=1,
                     A.vs.B=2,
                     amount=3,
                     A.vs.B.by.amount=4)))
options(old.width)
')


###################################################
### code chunk number 13: oway.tex:1355-1359
###################################################
hhpdf("catalystm-hov.pdf", width=7, height=4)
hovBF(concent ~ catalyst, data=catalystm)
hovplotBF(concent ~ catalyst, data=catalystm)
hhdev.off()


