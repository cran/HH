### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/twtb.tex'

###################################################
### code chunk number 1: twtb.tex:68-69
###################################################
require(vcd)


###################################################
### code chunk number 2: twtb.tex:91-95
###################################################
hhcapture("drunk.Rout", '
data(drunk)
drunk
')


###################################################
### code chunk number 3: twtb.tex:115-122
###################################################
hhpdf("drunk-prop-fem.pdf", width=5.5, height=3)
prop.female <- drunk["females",]/colSums(drunk)
ages <- ordered(dimnames(drunk)$age, levels=dimnames(drunk)$age)
barchart(prop.female ~ ages,
         horizontal=FALSE, origin=0,
         ylab="", main="proportion female")
hhdev.off()


###################################################
### code chunk number 4: twtb.tex:135-146
###################################################
hhpdf("drunk-mosaic.pdf", width=8, height=4)
mosaic(t(drunk), direction=c("v","h"),
       gp=gpar(fill=likertColor(2), col="transparent"),
       rot_labels=c(0,0,0,0),  ## zero is horizontal
       rot_varnames=c(0,0,0,0),
       offset_labels=c(0, -0.6, 0, 1),  ## top, right, bottom, left ## positive means outward
       offset_varnames=c(0, -0.6, 0, 2.4),
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE
)
hhdev.off()


###################################################
### code chunk number 5: twtb.tex:166-174
###################################################
hhcapture("drunk2.Rout", '
drunk.chisq <- chisq.test(drunk)
drunk.chisq
drunk.chisq$observed
drunk.chisq$expected
drunk.chisq$residuals   ## cell chi values
drunk.chisq$residuals^2 ## cell chi-square values
')


###################################################
### code chunk number 6: twtb.tex:200-208
###################################################
hhpdf("drunk-chi.pdf", width=5.5, height=3)
barchart(Freq ~ age | sex, as.data.frame(drunk.chisq$residuals),
         origin=0, layout=c(1,2), as.table=TRUE,
         scales=list(alternating=2), ##between=list(y=1),
         ylab=list("sex", rot=0),
         ylab.right=list("Chi values", rot=0), xlab="Age",
         strip=FALSE, strip.left=TRUE)
hhdev.off()


###################################################
### code chunk number 7: twtb.tex:302-311
###################################################
hhpdf("drunk-assoc.pdf", width=5.5, height=3.5)
assoc(drunk, gp=gpar(fill=likertColor(2)[2], col=0),
       margins=c(left=5),
      rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0),
      just_labels=c("center","right","center","right"),
      just_varnames=c("center","right","center","left"),
      offset_labels=c(0, 0, 0, 0),
      offset_varnames=c(0, 0, 0, 2.5))
hhdev.off()


###################################################
### code chunk number 8: twtb.tex:436-440
###################################################
hhcapture("glasses.Rout", '
data(glasses)
glasses
')


###################################################
### code chunk number 9: twtb.tex:469-473
###################################################
hhcapture("glasses2.Rout", '
fisher.test(glasses)
')
chisq.test(glasses, corr=FALSE)


###################################################
### code chunk number 10: twtb.tex:497-522
###################################################
hhcapture("glasses-all.Rout", '
## construct all possible two-way tables with the same margins as the
## initial table
all.tables <- function(x) {

  xx <- x

  r.margin <- rowSums(x)
  c.margin <- colSums(x)

  result <- array(0, dim=c(r.margin[1]+1, 2, 2),
                  dimnames=c(table=list(0:r.margin[1]), rev(dimnames(xx))))
  for (x11 in 0:r.margin[1]) {
    xx[1,1] <- x11
    xx[1,2] <- r.margin[1] - xx[1,1]
    xx[2,1] <- c.margin[1] - xx[1,1]
    xx[2,2] <- sum(x) - (xx[1,1] + xx[1,2] + xx[2,1])
    if (min(xx) >= 0) result[as.character(x11),,] <- t(xx)
  }
  result
}

glasses.all <- all.tables(glasses)
aperm(glasses.all, c(3,2,1))
')


###################################################
### code chunk number 11: twtb.tex:542-547
###################################################
hhpdf("glasses-exact.pdf", width=11, height=3)
(mosaic(glasses.all, direction=c("v","v","h"),
        highlighting=3, highlighting_fill=likertColor(2),
        spacing=spacing_increase(rate=c(.2, 3, 3.5))))
hhdev.off()


###################################################
### code chunk number 12: twtb.tex:567-597
###################################################
hhcapture("glasses-hypergeometric.Rout", '
g.p <- apply(glasses.all, 1,
             function(x)
               c(prob=dhyper(x[1,1], sum(x[1,]), sum(x[2,]), sum(x[,1])),
                 min=min(x)))
g.p2 <- data.frame(t(g.p), which=I(""))
## initial table
g.p2[as.character(glasses[1,1]),"which"] <- "*"
## more extreme tables (min value is smaller)
g.p2[g.p2[as.character(glasses[1,1]),"min"] > g.p2[,"min"],"which"] <- "<"
g.p2
g.p2$cumsum <- cumsum(g.p2$prob)
g.p2$rev.cumsum <- rev(cumsum(rev(g.p2$prob)))
g.p2
')
##
hhpdf("glasses-exact-prob.pdf", width=7, height=2.5)
barchart(g.p2$prob ~ factor(0:6), horizontal=FALSE,
         ylab=NULL, origin=0,
         main=list(labels=
           "probability of table with specified [1,1] position"),
         scales=list(x=list(at=1+0:6, labels=paste(0:6,g.p2$which))),
         xlab.top=list(format(round(g.p[1,], digits=4)), cex=.8),
         key=list(
           text=list(c("observed","more extreme")),
           text=list(c("*","<")),
           columns=2,
           border=TRUE,
           space="bottom"))
hhdev.off()


###################################################
### code chunk number 13: twtb.tex:689-733
###################################################
hhcapture("blyth.Rout", '
require(vcd)
require(abind)
require(reshape2)

data(blyth)

## rearrange as 3-way array
blyth3 <- blyth
dim(blyth3) <- c(2,2,2)
dimnames(blyth3) <- list(survival=c("not","survive"),
                         treatment=c("standard","new"),
                         location=c("A","B"))
blyth3x <- abind(blyth3,
                "A&B combined"=apply(blyth3, 1:2, sum))
names(dimnames(blyth3x)) <- names(dimnames(blyth3))
## blyth3x
structable(aperm(blyth3x, c(3,2,1)), direction=c("v","v","h"))


blyth3x.pct <- 100 * blyth3x /
  abind(apply(blyth3x, c(2,3), sum),
        apply(blyth3x, c(2,3), sum), along=.5)
round(
structable(aperm(blyth3x.pct, c(3,2,1)),
           direction=c("v","v","h"))
)

blyth3xdf <- cbind(as.data.frame.table(blyth3x),
                   Pct=as.vector(blyth3x.pct))
blyth3xdf$Survival <-
  factor(blyth3xdf$survival,
         levels=rev(levels(blyth3xdf$survival)))
blyth3xdf

blyth3xdf.1.8 <- blyth3xdf[1:8,]
blyth3xdf.1.8$location <- factor(blyth3xdf.1.8$location)

blyth3xdf.9.12 <- blyth3xdf[9:12,]
blyth3xdf.9.12$location <- factor(blyth3xdf.9.12$location)

blyth3xc <- dcast(location + treatment ~ survival,
                  value.var="Freq", data=blyth3xdf)
')


###################################################
### code chunk number 14: twtb.tex:767-777
###################################################
hhpdf("bC3r.pdf", width=7, height=2.5)
resizePanels(w=c(.31,.31,.38),
barchart(Freq ~ treatment | location, groups=Survival, data=blyth3xdf,
         stack=TRUE,
         horizontal=FALSE, ylab="Count",
         ylab.right=list(c("survive","not"), rot=0),
         col=likertColor(2)[2:1], border=likertColor(2)[2:1],
         layout=c(3,1), between=list(x=c(0,2)))
             )
hhdev.off()


###################################################
### code chunk number 15: twtb.tex:780-790
###################################################
hhpdf("bP3r.pdf", width=7, height=2.5)
resizePanels(w=c(.31,.31,.38),
barchart(Pct ~ treatment | location, groups=Survival, data=blyth3xdf,
         stack=TRUE,
         horizontal=FALSE, ylab="Percent",
         ylab.right=list(c("survive","not"), rot=0),
         col=likertColor(2)[2:1], border=likertColor(2)[2:1],
         layout=c(3,1), between=list(x=c(0,2)))
             )
hhdev.off()


###################################################
### code chunk number 16: twtb.tex:793-804
###################################################
hhpdf("bP3s.pdf", width=7, height=2.5)
print(position=c(0, 0, .93, 1),
resizePanels(w=c(.31,.31,.38),
barchart(Pct ~ treatment | location, data=blyth3xdf,
         subset=(survival=="survive"),
         origin=0, ylim=c(-7, 107), col=likertColor(2)[2], border="white",
         ylab="Percent Survive",
         layout=c(3,1), between=list(x=c(0,2)))
             )
)
hhdev.off()


###################################################
### code chunk number 17: twtb.tex:807-820
###################################################
## Figures mc2.pdf and mc1.pdf have the panel borders drawn by strucplot
## and the labeling by mosaic inside each panel.  They are positioned by LaTeX.
hhpdf("mc2.pdf", width=6, height=3)
cotabplot(~ treatment + survival | location, data=blyth3xdf.1.8,
          layout = c(1, 2), direction=c("v","h"),
          rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), ## zero is horizontal
          offset_labels=c(0, -0.6, 0, .5),  ## top, right, bottom, left ## positive means outward
          offset_varnames=c(0, -0.6, 0, 2),
          panel_args=list(margins=c(4,1,2,6)),  ## need room for horizontal variable name
          keep_aspect_ratio=FALSE,
          spacing=spacing_highlighting(rate=6),
          gp=gpar(fill=likertColor(2), col=0))
hhdev.off()


###################################################
### code chunk number 18: twtb.tex:822-833
###################################################
hhpdf("mc1.pdf", width=4.2, height=3)
cotabplot(~ treatment + survival | location, data=blyth3xdf.9.12,
          layout = c(1, 2), direction=c("v","h"),
          rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), ## zero is horizontal
          offset_labels=c(0, -0.6, 0, .5),  ## top, right, bottom, left ## positive means outward
          offset_varnames=c(0, -0.6, 0, 2),
          panel_args=list(margins=c(4,1,2,6)),  ## need room for horizontal variable name
          keep_aspect_ratio=FALSE,
          spacing=spacing_highlighting(rate=6),
          gp=gpar(fill=likertColor(2), col=0))
hhdev.off()


###################################################
### code chunk number 19: twtb.tex:836-858
###################################################
## Figures mc3a.pdf and mc3b.pdf have the panel borders drawn by lattice
## and the labeling outside all panels.  The figures are manually superposed using LaTeX.
## The offsets, margins and such are carefully tailored to these pdf settings.
mosaic.labels <- TRUE  ## to see a completely labeled mosaic plot
mosaic.labels <- FALSE ## to see a mosaic plot with all labels suppressed
{if (mosaic.labels)
   hhpdf("mc3.pdf", width=7, height=2.25)
else
  hhpdf("mc3a.pdf", width=7, height=2.25)
}
mosaic(~ treatment + survival | location, data=blyth3xdf,
       layout = c(1, 3), direction=c("v","v","h"),
       rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), ## zero is horizontal
       offset_labels=c(0, -0.6, 0, 1.5),  ## top, right, bottom, left ## positive means outward
       offset_varnames=c(0, -0.6, 0, 2.8),
       varnames=mosaic.labels,
       labels=mosaic.labels,
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE,
       spacing=spacing_highlighting(rate=3.5),
       gp=gpar(fill=likertColor(2), col=0))
hhdev.off()


###################################################
### code chunk number 20: twtb.tex:860-872
###################################################
hhpdf("mc3b.pdf", width=8.5, height=2.5)
print(position=c(0, 0, .93, 1),
resizePanels(w=c(.28, .26, .46),
barchart(Pct ~ treatment | location, data=blyth3xdf,
         subset=(survival=="survive"),
         origin=0, ylim=c(-7, 107), col=0, border=0,
         ylab="Percent",
         ylab.right=list(c("survive","not"), rot=0),
         layout=c(3,1), between=list(x=c(0,2)))
             )
)
hhdev.off()


###################################################
### code chunk number 21: twtb.tex:875-903
###################################################
## Figure mc3pdf.pdf is merged at the R level.
## The offsets, margins and such are carefully tailored to these pdf settings.
hhpdf("mc3pdf.pdf", width=7, height=2.5)
##
mosaic(~ treatment + survival | location, data=blyth3xdf,
       layout = c(1, 3), direction=c("v","v","h"),
       rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), ## zero is horizontal
       offset_labels=c(0, -0.6, 0, 1.5),  ## top, right, bottom, left ## positive means outward
       offset_varnames=c(0, -0.6, 0, 2.8),
       varnames=mosaic.labels,
       labels=mosaic.labels,
       margins=c(top=3.45, right=6, bottom=2.935, left=5.5),
       keep_aspect_ratio=FALSE,
       spacing=spacing_highlighting(rate=3.5),
       gp=gpar(fill=likertColor(2), col=0))
##
print(more=TRUE,
resizePanels(w=c(.305, .26, .435),
barchart(Pct ~ treatment | location, data=blyth3xdf,
         subset=(survival=="survive"),
         origin=0, ylim=c(-7, 107), col=0, border=0,
         ylab="Percent",
         ylab.right=list(c("survive","not"), rot=0),
         layout=c(3,1), between=list(x=c(0, 1)))
             )
)
##
hhdev.off()


###################################################
### code chunk number 22: twtb.tex:906-912
###################################################
hhpdf("lC3r.pdf", width=7, height=2.5)
likert(treatment ~ .| location, blyth3xc, horizontal=FALSE,
       main=NULL,  xlab=NULL,
       layout=c(3,1), between=list(x=c(0,2)), w.resizePanels=c(.31,.31,.38),
       ylab.right=list(c("not","survive"), rot=0), auto.key=FALSE)
hhdev.off()


###################################################
### code chunk number 23: twtb.tex:915-921
###################################################
hhpdf("lP3r.pdf", width=7, height=2.5)
likert(treatment ~ .| location, blyth3xc, horizontal=FALSE, as.percent=TRUE,
       main=NULL, xlab=NULL,
       layout=c(3,1), between=list(x=c(0,2)), w.resizePanels=c(.31,.31,.38),
       ylab.right=list(c("not","survive"), rot=0), auto.key=FALSE)
hhdev.off()


###################################################
### code chunk number 24: twtb.tex:1238-1240
###################################################
hhpdf("myfile.pdf", width=5.5, height=5.5)
hhdev.off()


###################################################
### code chunk number 25: twtb.tex:1283-1285
###################################################
hhcapture("myfile.Rout", '
')


###################################################
### code chunk number 26: twtb.tex:1309-1318
###################################################
hhcapture("hypothermia.Rout", '
hypothermia <-
    matrix(c(83,61,54,75), byrow=TRUE,
           nrow=2,
           dimnames=list(
             Outcome=c("not.favorable","favorable"),
             Treatment=c("control","treated")))
hypothermia
')


###################################################
### code chunk number 27: twtb.tex:1332-1336
###################################################
hhpdf("hypothermiamosaic.pdf", width=4, height=3.5)
mosaic(Outcome ~ Treatment, data=as.data.frame.table(hypothermia), direction=c("v","h"),
       gp=gpar(fill=likertColor(2), col="white"), keep_aspect_ratio=FALSE)
hhdev.off()


###################################################
### code chunk number 28: twtb.tex:1347-1357
###################################################
hhpdf("hypothermiacount.pdf", width=5, height=3)
## not included in book
hypothermia.df <- as.data.frame.table(hypothermia)
hypothermia.df$Outcome <-
  factor(hypothermia.df$Outcome, levels=rev(levels(hypothermia.df$Outcome)))

barchart(Freq ~ Treatment, groups=Outcome, stack=TRUE, hypothermia.df,
         horizontal=FALSE, origin=0, ylab="Count",
         auto.key=list(border=TRUE, space="right", reverse=TRUE))
hhdev.off()


###################################################
### code chunk number 29: twtb.tex:1369-1379
###################################################
hhpdf("hypothermiaproportion.pdf", width=5, height=3)
## not included in book
hypothermiaProportion.df <- as.data.frame.table(hypothermia / rep(colSums(hypothermia), each=2))
hypothermiaProportion.df$Outcome <-
  factor(hypothermiaProportion.df$Outcome, levels=rev(levels(hypothermiaProportion.df$Outcome)))

barchart(Freq ~ Treatment, groups=Outcome, stack=TRUE, hypothermiaProportion.df,
         horizontal=FALSE, origin=0, ylab="Proportion",
         auto.key=list(border=TRUE, space="right", reverse=TRUE))
hhdev.off()


###################################################
### code chunk number 30: twtb.tex:1391-1400
###################################################
hhpdf("hypothermiaodds.pdf", width=3.5, height=2.5)
barchart(hypothermia[2,] / hypothermia[1,] ~ dimnames(hypothermia)[[2]],
          horizontal=FALSE, origin=0, ylab="odds favorable")
hhdev.off()
##
hhpdf("hypothermialogit.pdf", width=3.5, height=2.5)
barchart(log(hypothermia[2,] / hypothermia[1,]) ~ dimnames(hypothermia)[[2]],
          horizontal=FALSE, origin=0, ylab="logit favorable")
hhdev.off()


###################################################
### code chunk number 31: twtb.tex:1510-1513
###################################################
hhpdf("hypothermiaplotOddsRatio.pdf", width=7.5, height=5)
plotOddsRatio(t(hypothermia))
hhdev.off()


###################################################
### code chunk number 32: twtb.tex:1653-1661
###################################################
hhpdf("salkMosaic.pdf", width=12, height=3.5)
data(salk)

(mosaic(Freq ~ vaccine + paralyze | age, data=salk, direction=c("v","v","h"),
        main="Observed number of observations in each age group",
        gp=gpar(fill=likertColor(2)[2:1], col=0),
        spacing=spacing_increase(rate=c(.4, 1.4, 3.5))))
hhdev.off()


###################################################
### code chunk number 33: twtb.tex:1675-1684
###################################################
hhcapture("MHsalk.Rout", '
## Code for calculation of the Cochran--Mantel--Haenszel test of the polio example
salk2 <- tapply(salk$Freq, salk[c(2,3,1)], c)
class(salk2) <- "table"
salk2

mantelhaen.test(salk2)
mantelhaen.test(salk2, correct=FALSE)
')


###################################################
### code chunk number 34: twtb.tex:1688-1768
###################################################
hhcapture("arithMHsalk.Rout", '
## Code for "Detail for calculation of the Cochran--Mantel--Haenszel test of the polio example."
## counts
salk2

## proportion without paralysis
pp <- apply(salk2, c(3,1),
            function(x) x[1]/(x[1]+x[2]))
pp

## binomial variance for proportion without paralysis
apply(salk2, c(3,1),
      function(x) (x[1]/(x[1]+x[2]))*(x[1]/(x[1]+x[2])) / (x[1]+x[2]))


## average proportion without paralysis
p <- apply(salk2, 3,
      function(x) sum(x[,1])/sum(x))
p

## weight per table
w <- apply(salk2, 3,
           function(x) 1/sum(1/(x[,1]+x[,2])))
w

## diff of proportion without paralysis
dp <- pp[,1] - pp[,2]
dp

## binomial variance for difference of proportions without paralysis
p*(1-p)

sum(w*dp) / sqrt(sum(w*p*(1-p)))


## chi-square for each table
chisq.table <-
t(apply(salk2, 3,
      function(x) {
        e <- (x[,1]+x[,2]) %o% (x[1,]+x[2,]) / sum(x)
        chisq <- sum((x-e)^2/e)
        p <- 1-pchisq(chisq,1)
        c(chisq=chisq, p.chisq=p)
      }))
chisq.table

## expected counts under independence for each table
E <- apply(salk2, 3,
           function(x) {
             (x[,1]+x[,2]) %o% (x[1,]+x[2,]) / sum(x)
           })
dimnames(E) <- NULL
dim(E) <- dim(salk2)
dimnames(E) <- dimnames(salk2)
E

## mh chi-square for each table (hypergeometric assumption)
apply(salk2, 3,
      function(x) {
        e <- (x[,1]+x[,2]) %o% (x[1,]+x[2,]) / sum(x)
        v <- prod(x[,1]+x[,2], x[1,]+x[2,]) / (sum(x)^2 * (sum(x)-1))
        (x-e)[1,1]^2 / v
      })


## Mantel-Haenszel chi-square components for each table
## (hypergeometric assumption)
mh.c <-
t(apply(salk2, 3,
      function(x) {
        e <- (x[,1]+x[,2]) %o% (x[1,]+x[2,]) / sum(x)
        v <- prod(x[,1]+x[,2], x[1,]+x[2,]) / (sum(x)^2 * (sum(x)-1))
        c(O=x[1,1], E=e[1,1], O.E=(x-e)[1,1], v=v, n=sum(x),
          dev=(x-e)[1,1]/sqrt(v), mh=(x-e)[1,1]^2 / v)
      }))
mh.c

## Cochran-Mantel-Haenszel test statistic
sum(mh.c[,"O.E"])^2 / sum(mh.c[,"v"])
')


###################################################
### code chunk number 35: twtb.tex:1894-1905
###################################################
hhpdf("salk-dev.pdf", width=7.5, height=4)
ages <- ordered(dimnames(mh.c)[[1]], levels=dimnames(mh.c)[[1]])
barchart(mh.c[,"dev"] ~ ages, origin=0, horizontal=FALSE, border="white",
         xlab="Age Group", xlab.top="Number of Observations",
         scales=list(cex=1), ylab="standardized table deviations",
         par.settings=list(clip=list(panel=FALSE)),
         panel=function(...) {
           panel.barchart(...)
           panel.axis("top", labels=mh.c[,"n"], outside=TRUE, ticks=FALSE, rot=0)
         })
hhdev.off()


###################################################
### code chunk number 36: twtb.tex:1961-1970
###################################################
hhcapture("salkFisher.Rout", '
data(salk)
salk2 <- tapply(salk$Freq, salk[c(2,3,1)], c)
class(salk2) <- "table"
## salk2
lt <- apply(salk2, 3, fisher.test, alternative="less")
## odds ratio and p-value
sapply(lt, `[`, c("estimate","p.value"))
')


###################################################
### code chunk number 37: twtb.tex:2180-2182
###################################################
hhcapture("myfile.Rout", '
')


