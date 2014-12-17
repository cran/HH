### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/conc.tex'

###################################################
### code chunk number 1: conc.tex:138-152
###################################################
BB <- matrix(c(2,4,4,5), 2, 2,
             dimnames=list(First=c("R*","W*"), Second=c("*R","*W")))
require(vcd)
hhpdf("RW.pdf", height=4, width=4)
mosaic(BB,
       rot_labels=c(0,0,0,0),  ## zero is horizontal
       rot_varnames=c(0,0,0,0),
       gp=gpar(fill=c("red","#ff8888","#ff8888","white"), col="black"),
       spacing=spacing_equal(sp=unit(0,"lines")))
grid.text("A", x=.92, y=.36)
grid.text(expression(bar(A)), x=.92, y=.70)
grid.text("B", x=.63, y=.07)
grid.text(expression(bar(B)), x=.25, y=.07)
hhdev.off()


###################################################
### code chunk number 2: conc.tex:246-251
###################################################
hhpdf("discuniv.pdf", width=3, height=2)
discuniv <- data.frame(y=c(.25, .50, .25), x=factor(0:2))
barchart(y ~ x, data=discuniv, ylab="P(X = x)", xlab="x", origin=0, col="#ff4444",
         scales=list(y=list(at=c(0, .25, .50))))
hhdev.off()


###################################################
### code chunk number 3: conc.tex:287-314
###################################################
require(reshape2)
pmf <- matrix(c(.10, .05, .20, .10, .30, .25), 2, 3,
              dimnames=list(x=1:2, y=0:2))
hhpdf("discbiv1.pdf", width=2.5, height=2)
barchart(value ~ rep("", 6) | y*x,
         data=melt(pmf, as.is=TRUE),
         origin=0, box.ratio=100,
         scales=list(x=list(limits=c(.65, 1.35), alternating=0),
                     y=list(relation="free", limits=c(0,.35),
                            at=.15, labels=list("1", "","","2","",""))),
         strip=FALSE,
         par.settings=list(axis.line=list(col=0), clip=list(panel=FALSE)),
         as.table=TRUE, col="#ff4444",
         xlab.top="y", ylab=list("x", rot=0)) +
    layer(panel.axis(side="top", at=1, rot=0, tck=0, outside=TRUE,
          labels=c("0","1","2","","","")[panel.number()]))
hhdev.off()
hhpdf("discbiv2.pdf", width=3, height=2)
mosaic(t(pmf), split_vertical=c(TRUE,FALSE),
       highlighting=2, highlighting_fill=c("#ff8888","red"),
       rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), keep_aspect_ratio=FALSE)
hhdev.off()
hhpdf("discbiv3.pdf", width=3, height=2)
mosaic(pmf,
       highlighting=2, highlighting_fill=c("#ffBBBB","#ff8888","red"),
       rot_labels=c(0,0,0,0), rot_varnames=c(0,0,0,0), keep_aspect_ratio=FALSE)
hhdev.off()


###################################################
### code chunk number 4: conc.tex:414-432
###################################################
hhpdf("bimodal-shade.pdf", height=4, width=7)
xx <- seq(-3, 6, .025)
dd <- (dnorm(xx, mean=0, sd=1) + dnorm(xx, mean=2.5, sd=1.1))/2
pr <- (pnorm(c(2,4), mean=0, sd=1) + pnorm(c(2,4), mean=2.5, sd=1.1))/2
xyplot(dd ~ xx, type="l",
       panel=function(x, y, ...) {
         grid.polygon(x=x[c(201,201:281,281)],
                      y=c(0,y[201:281],0),
                      gp=gpar(fill="gray60", col="gray60"),
                      default.units="native")
          panel.xyplot(x, y, ...)
         panel.abline(h=0, lty=1, col="gray70")
      },
       ylab=list("f(x)", rot=0),
       xlab=list("x"),
       main=list(paste("Prob(2 < X < 4) =", round(diff(pr),3)))
       )
hhdev.off()


###################################################
### code chunk number 5: conc.tex:591-608
###################################################
pp <- ppoints(101)

zz.norm <- qnorm(pp, s=2.2)
dd.norm <- dnorm(zz.norm, s=2.2)
N <- xyplot(dd.norm ~ zz.norm, type="l") + layer(panel.abline(h=0, col="gray70"))

xx.chisq4 <- c(0,qchisq(pp, 4))
dd.chisq4 <- dchisq(xx.chisq4, 4)
C4 <- xyplot(dd.chisq4 ~ xx.chisq4, type="l") + layer(panel.abline(h=0, col="gray70"))
C4R <- xyplot(rev(dd.chisq4) ~ -rev(xx.chisq4), type="l") + layer(panel.abline(h=0, col="gray70"))

hhpdf("skewdens2.pdf", height=3.5, width=8)
update(c("negatively skewed"=C4R,
         symmetric=N, "positively skewed"=C4,
         layout=c(3,1), y.same=TRUE),
       between=list(x=1), xlab="x", ylab="density")
hhdev.off()


###################################################
### code chunk number 6: conc.tex:661-670
###################################################
hhcapture("tv-freq.Rout", '
data(tv)
tmp <- as.matrix(table(cut(tv$male.life.exp, breaks=seq(49.5,79.5,5))))
dimnames(tmp) <-
    list("Male Life Expectancy"=
              c("50--54","55--59","60--64","65--69","70--74","75--79"),
         " "="Frequency")
tmp
')


###################################################
### code chunk number 7: conc.tex:712-716
###################################################
hhpdf("tv-hist.pdf", height=3.5, width=7)
histogram( ~ male.life.exp, data = tv,
          breaks=seq(49.5, 79.5, 5), type="count", col="gray60")
hhdev.off()


###################################################
### code chunk number 8: conc.tex:758-761
###################################################
hhcapture("conc-stem-male.Rout", '
stem(tv$male.life.exp)
')


###################################################
### code chunk number 9: conc.tex:857-860
###################################################
hhcapture("conc-quartiles-male.Rout", '
quantile(tv$male.life.exp)
')


###################################################
### code chunk number 10: conc.tex:871-874
###################################################
hhpdf("tv-bw.pdf", height=2.5, width=7)
bwplot( ~ male.life.exp, data = tv)
hhdev.off()


###################################################
### code chunk number 11: conc.tex:891-925
###################################################
pp <- ppoints(100)
pp <- c(0, pp[1:25], .25, pp[26:50], .50, pp[51:75], .75, pp[76:100])
q.pp <- qf(pp, df1=3, df2=36)
dd <- df(q.pp, df1=3, df2=36)

hhpdf("quartiles.pdf", height=4.5, width=7)
     xyplot(dd ~ q.pp, type="l",
            par.settings = list(clip = list(panel = "off")),
ylim=c(-.1, .78),
            panel=function(x, y, ...) {
              grid.polygon(x=x[c(1,1:27,27)],
                           y=c(0,y[1:27],0),
                           gp=gpar(fill="gray60", col="gray60"), default.units="native")
              grid.polygon(x=x[c(53,53:79,79)],
                           y=c(0,y[53:79],0),
                           gp=gpar(fill="gray60", col="gray60"), default.units="native")
              panel.axis("bottom", at=x[c(27,53,79)],
                         tck = .5, # line.col = "transparent",
                         labels=c("Q1","Med","Q3"),
                         half=FALSE,
                         rot=0, text.cex=1)
              panel.axis("bottom", at=x[c(27,53,79)],
                         tck = 2.5,
                         labels=round(x[c(27,53,79)], 2),
                         outside=TRUE,
                         rot=0, text.cex=1)
              panel.abline(h=0, col="gray70")
              panel.xyplot(x, y, ...)
            },
            ylab=list("density", cex=1.6),
            xlab=list("quantile", cex=1.6),
            main=list("Quartiles of F(3,36)", cex=1.5)
            )
hhdev.off()


###################################################
### code chunk number 12: conc.tex:943-962
###################################################
hhpdf("skew.pdf", height=3, width=7)
sym <- rnorm(100)

neg <- sym[sym<0]
pos <- sym[sym>0]

neg.skew <- c(-neg^2, pos^.5)

pos.skew <- c(-(-neg)^.5, pos^2)

skew.levels <- c("negatively skewed", "symmetric", "positively skewed")
skew.df <- data.frame(y=c(neg.skew, sym, pos.skew),
                      dist=ordered(rep(skew.levels, c(100,100,100)),
                        levels=skew.levels))

bwplot(dist ~ y, data=skew.df, xlab="",
 par.settings=list(plot.symbol=list(pch=19),
                   box.dot=list(pch=19, col=trellis.par.get()$plot.symbol$col)))
hhdev.off()


###################################################
### code chunk number 13: conc.tex:1035-1049
###################################################
hhpdf("corr-eps.pdf", height=2.5, width=8)
x <- rnorm(100)
e <- rnorm(100)
r <- c(-1, -.9, -.5, 0, .5, .9, 1)

corr.data <- data.frame(e, x, x %o% r + e %o% (1-r^2)^.5)
names(corr.data)[3:9] <- r

corr.data.melt <- reshape2::melt(corr.data[,-1], id="x", variable.name="correlation", value.name="y")
maxabs <- c(-1,1) * 3.9
xyplot(y ~ x | correlation, data=corr.data.melt, layout=c(7, 1), aspect="iso",
       xlim=maxabs, ylim=maxabs, ylab=list(rot=0), scales=list(at=c(-2,0,2), alternating=FALSE),
       strip=strip.custom(var.name=expression(rho), strip.names=c(TRUE,TRUE), sep=" = "))
hhdev.off()


###################################################
### code chunk number 14: conc.tex:1092-1099
###################################################
hhpdf("bivnorm8.pdf", height=8, width=8)
bv8 <- bivariateNormal(.7)  ## all views on one page
bv8
hhdev.off()
hhpdf("bivnorm1125.pdf", height=4, width=4)
update(bv8[3], layout=c(1,1))
hhdev.off()


###################################################
### code chunk number 15: conc.tex:1278-1284
###################################################
hhpdf("norm.pdf", height=4.5, width=7)
tmp.norm <-
NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,  xlim=c(75, 125), sd=5,
               digits=6, zaxis=TRUE, cex.z=0.6, cex.prob=.9)
print(tmp.norm, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 16: conc.tex:1286-1289
###################################################
hhcapture("norm.Rout", '
attr(tmp.norm, "scales")
')


###################################################
### code chunk number 17: conc.tex:1319-1353
###################################################
hhpdf("tt.pdf", height=8, width=8)
normal  <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                          xlim=c(75, 125), sd=5, digits=6, distribution="z",
                          zaxis=TRUE, cex.z=0.6, cex.prob=.9,
                          key.axis.padding=6)
`t[30]` <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                          xlim=c(75, 125), sd=5, digits=6, distribution="t", df=30,
                          zaxis=TRUE, cex.z=0.6, cex.prob=.9)
`t[10]` <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                          xlim=c(75, 125), sd=5, digits=6, distribution="t", df=10,
                          zaxis=TRUE, cex.z=0.6, cex.prob=.9)
`t[2]`  <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                          xlim=c(75, 125), sd=5, digits=6, distribution="t", df=2,
                          zaxis=TRUE, cex.z=0.6, cex.prob=.9)

scales <-
cbind(
normal  = attr(normal , "scales")[,2],
`t[30]` = attr(`t[30]`, "scales")[,2],
`t[10]` = attr(`t[10]`, "scales")[,2],
`t[2]`  = attr(`t[2]` , "scales")[,2]
)
rownames(scales) <- c(colnames(attr(normal , "scales"))[2], "t")

NTTT <-
  update(
    c(normal=normal, "t[30]"=`t[30]`, "t[10]"=`t[10]`, "t[2]"=`t[2]`,
      layout=c(2,2), y.same=TRUE, x.same=FALSE),
    strip=FALSE, strip.left=TRUE, scales=list(y=list(alternating=1)),
    between=list(x=2, y=4), ylab="density",
    main=expression("normal and three t distributions, " ~ sigma[bar(x)]==5 ~ "," ~ n==1)) +
  layer(panel.abline(h=.08, col="gray60", lty=2, lwd=.5))
print(NTTT, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 18: conc.tex:1355-1358
###################################################
hhcapture("tt.Rout", '
scales
')


###################################################
### code chunk number 19: conc.tex:1433-1468
###################################################
hhpdf("normalCLT.pdf", height=8, width=8)
n1  <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                      xlim=c(85, 115), sd=5, n=1,  digits=6,
                      zaxis=TRUE, cex.z=0.6, cex.prob=.9, ylim=c(0, .65),
                      key.axis.padding=6)
n4  <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                      xlim=c(85, 115), sd=5, n=4,  digits=6,
                      zaxis=TRUE, cex.z=0.6, cex.prob=.9, ylim=c(0, .65))
n16 <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                      xlim=c(85, 115), sd=5, n=16, digits=6,
                      zaxis=list(at=seq(85, 115, 5), labels=seq(-12, 12, 4)),
                      cex.z=0.6, cex.prob=.9, ylim=c(0, .65))
n64 <- NormalAndTplot(mean0=100, mean1=NA,  xbar=NA,
                      xlim=c(85, 115), sd=5, n=64, digits=6,
                      zaxis=list(at=seq(85, 115, 5), labels=seq(-24, 24, 8)),
                      cex.z=0.6, cex.prob=.9, ylim=c(0, .65))

scales <-
cbind(
"n=1"  = attr(n1 , "scales")[,2],
"n=4"  = attr(n4 , "scales")[,2],
"n=16" = attr(n16, "scales")[,2],
"n=64" = attr(n64, "scales")[,2]
)
rownames(scales)[1] <- c(colnames(attr(n1 , "scales"))[2])

NNNN <-
  update(
    c("n=1"=n1, "n=4"=n4, "n=16"=n16, "n=64"=n64,
      layout=c(2,2), y.same=TRUE, x.same=FALSE),
    strip=FALSE, strip.left=TRUE, scales=list(y=list(alternating=1)),
    between=list(x=2, y=4), ylab="density",
    main=expression("Normal with increasing sample size n, " ~ sigma[bar(x)]==5))
print(NNNN, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 20: conc.tex:1470-1473
###################################################
hhcapture("normalCLT.Rout", '
scales
')


###################################################
### code chunk number 21: conc.tex:1633-1641
###################################################
hhpdf("normalconf.pdf", height=5, width=8)
tmp <- NormalAndTplot(xbar=8.5, sd=2, df=24, n=25,
                      xlim=c(7,10), ylim=c(0,0.96),
                      alpha.right=0.025, alpha.left=0.025,
                      distribution.name="t", type="confidence",
                      z1axis=TRUE, zaxis=TRUE, cex.z=0.7, cex.prob=1)
print(tmp, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 22: conc.tex:1643-1646
###################################################
hhcapture("normalconf.Rout", '
attr(tmp, "scales")
')


###################################################
### code chunk number 23: conc.tex:1986-1993
###################################################
hhpdf("bottlefill.pdf", height=5, width=8)
tmp <- NormalAndTplot(mean0=32, xbar=31.94, sd=.3, n=100,
                      xlim=c(31.88,32.12), ylim=c(0,13),
                      alpha.right=0.005, alpha.left=0.005,
                      zaxis=TRUE, cex.z=0.7)
print(tmp, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 24: conc.tex:1995-2000
###################################################
old.width <- options(width=70)
hhcapture("bottlefill.Rout", '
attr(tmp, "scales")
')
options(old.width)


###################################################
### code chunk number 25: conc.tex:2059-2066
###################################################
hhpdf("bottlefillonetail.pdf", height=5, width=8)
tmp <- NormalAndTplot(mean0=32, xbar=31.94, sd=.3, n=100,
                      xlim=c(31.88,32.12), ylim=c(0,13),
                      alpha.right=0, alpha.left=0.01,
                      zaxis=TRUE, cex.z=0.7)
print(tmp, tables=FALSE)
hhdev.off()


###################################################
### code chunk number 26: conc.tex:2068-2073
###################################################
old.width <- options(width=70)
hhcapture("bottlefillonetail.Rout", '
attr(tmp, "scales")
')
options(old.width)


###################################################
### code chunk number 27: conc.tex:2178-2266
###################################################
tmp8     <- NormalAndTplot(mean0=8, mean1=8, sd=2, n=64,
                           xlim=c(7.3, 9.5), cex.top.axis=1.8, cex.prob=1.5,
                           cex.z=0.7, prob.labels=FALSE,
                           digits.axis=5, digits.float=3)
## tmp8

tmp8411  <- NormalAndTplot(mean0=8, mean1=8.411, sd=2, n=64,
                           xlim=c(7.3, 9.5), cex.top.axis=1.8, cex.prob=1.5,
                           cex.z=0.7, prob.labels=FALSE,
                           digits.axis=5, digits.float=3)
## tmp8411

tmp87314 <- NormalAndTplot(mean0=8, mean1=8.7314, sd=2, n=64,
                           xlim=c(7.3, 9.5), cex.top.axis=1.8, cex.prob=1.5,
                           cex.z=0.7, prob.labels=FALSE,
                           digits.axis=5, digits.float=3)
## tmp87314

## > rgb(t(col2rgb("pink")), maxColorValue=255)
## [1] "#FFC0CB"

ntp8     <- NormalAndT.power(tmp8,    digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntp8
ntp8411  <- NormalAndT.power(tmp8411, digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntp8411
ntp87314 <- NormalAndT.power(tmp87314,  digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntp87314

ntb8     <- NormalAndT.power(tmp8,     which="beta",
                             digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntb8
ntb8411  <- NormalAndT.power(tmp8411,  which="beta",
                             digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntb8411
ntb87314 <- NormalAndT.power(tmp87314, which="beta",
                             digits.top.axis=5, digits.left=3,
                             col.power="#FF4040", cex.top.axis=1.8,
                             lwd.line=3, cex.left.axis=1.8)
## ntb87314

hhpdf("power64.pdf", height=7.5, width=17) ## density and power: not in book
print(position=c(.034, 0, .334, .95), more=TRUE,
      update(NormalAndT.and.power(tmp8,    pnt=ntp8,      display.ylab=FALSE),
             ylab=NULL, main=NULL),
      tables=FALSE)
print(position=c(.367, 0, .667, .95), more=TRUE,
      update(NormalAndT.and.power(tmp8411, pnt=ntp8411,   display.ylab=FALSE),
             ylab=NULL, main=NULL),
      tables=FALSE)
print(position=c(.700, 0, 1.000, .95), more=FALSE,
      update(NormalAndT.and.power(tmp87314, pnt=ntp87314, display.ylab=FALSE),
             ylab=NULL, main=NULL),
      tables=FALSE)
hhdev.off()


hhpdf("powerbeta64.pdf", height=11, width=19) ## density, power, beta: in book
combine3 <- function(tmp8, ntp8, ntb8) {
  tmp <-
    resizePanels(h=c(.6,.2,.2),
                 update(c(normal=update(tmp8, main=NULL, ylab=NULL, xlab=NULL,
                            scales=list(y=list(rot=0))),
                          power=ntp8,
                          beta=ntb8,
                          layout=c(1, 3),
                          y.same=FALSE, x.same=TRUE),
                        strip=FALSE, strip.left=FALSE, between=list(y=4),
                        as.table=TRUE))
  class(tmp) <- "trellis"
  tmp
}
AA <- combine3(tmp8, ntp8, ntb8)
BB <- combine3(tmp8411, ntp8411, ntb8411)
CC <- combine3(tmp87314, ntp87314, ntb87314)
print(position=c(.034, 0,  .334, .95), more=TRUE,  AA)
print(position=c(.367, 0,  .667, .95), more=TRUE,  BB)
print(position=c(.700, 0, 1.000, .95), more=FALSE, CC)
hhdev.off()


###################################################
### code chunk number 28: conc.tex:2327-2368
###################################################
oc.data <- data.frame(mu=seq(7.8, 9, .01))

COMMON <-
xyplot(pnorm(33.644 - 4*mu) ~ mu, data=oc.data,
       main="Operating Characteristics Curve",
       par.settings = list(clip=list(panel="off"),
                           layout.widths=list(axis.right=1.1),
                           layout.heights=list(axis.top=1.3)),
       xlab=expression("True value of" ~ mu),
       ylab=expression(beta(mu) == "P(Retain" ~  H[0] * " | True value of" ~ mu * ")"),
       ylim=c(0, 1), scales=list(y=list(at=seq(0, 1, .2), labels=format(seq(0, 1, .2), nsmall=2))),
       sub=expression(H[0] * ":" ~ mu ~ "" <= "" ~ 8 ~ ", " ~ alpha == .05 * ", " ~
       n == 64 * ", " ~  sigma == 2 * ", " ~ mu[c] == 8.411),
       type="l",
       panel=function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(h=.95, v=8, lty=5, lwd=.5, col="red")
         panel.abline(h=.5, v=8.411, lty=2, lwd=.5, col="blue")
         panel.axis("top", at=c(8, 8.411), labels=c(expression(mu[0]==8), expression(mu[c]==8.411)),
                    half=FALSE,
                    tck = 1.5,
                    outside=TRUE,
                    tick=TRUE, text.cex=1, rot=0)
       })

OC <- COMMON +
        layer(panel.axis("right", at=c(0.5, 0.95), labels=format(c(.5, .95), nsmall=2),
                         half=FALSE, outside=TRUE))

POWER <-
update(COMMON,
       main="Power Curve",
       ylim=c(1, 0), scales=list(y=list(at=seq(1, 0, -.2), labels=format(seq(0, 1, .2), nsmall=2))),
       ylab=expression("Power = P(Reject" ~  H[0] * " | True value of" ~ mu * ")")) +
         layer(panel.axis("right", at=c(0.5, 0.95), labels=format(c(.5, .05), nsmall=2),
                          half=FALSE, outside=TRUE))

hhpdf("ocp.pdf", height=4, width=8)
print(OC,    more=TRUE,  position=c(.00, 0,  .48, 1))
print(POWER, more=FALSE, position=c(.52, 0, 1.00, 1))
hhdev.off()


