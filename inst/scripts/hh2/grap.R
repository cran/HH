### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/grap.tex'

###################################################
### code chunk number 1: grap.tex:53-64
###################################################
data(njgolf)
hhpdf("grap-pric-lot.pdf", width=7, height=4)
col.lot <- trellis.par.get("superpose.symbol")$col[1:2]
xyplot(sprice ~ lotsize, data=njgolf, groups=(lotsize==0), pch=c("+","0"), cex=c(2, 1.8),
       ylab="Selling Price", xlab="Lot Size\n", col=col.lot[2:1],
       main="Single Family Homes",
       key=list(
         border=TRUE, space="bottom", columns=2,
         points=list(pch=c("0","+"), col=col.lot, cex=1.5),
         text=list(c("Zero","Positive"), col=col.lot)))
hhdev.off()


###################################################
### code chunk number 2: grap.tex:137-162
###################################################
eco.corr <- data.frame(x=1:99,
                       g=factor(rep(1:3, c(33,33,33))),
                       eps=rnorm(99))
eco.corr$y <- 30*as.numeric(eco.corr$g) - .3*eco.corr$x + 8*eco.corr$eps

cor(eco.corr$x, eco.corr$y)

for (i in 1:3) print(cor(eco.corr$x[eco.corr$g==i], eco.corr$y[eco.corr$g==i]))

xyplot(y ~ x, group=g, data=eco.corr, panel=panel.superpose, pch=c(15,19,17))

summary(lm(y ~ x, data=eco.corr), corr=FALSE)

summary(lm(y ~ g + x, data=eco.corr), corr=FALSE)
hhpdf("grap-ecop%03d.pdf", width=7, height=4.3, onefile=FALSE)
## Top two panels are on their own page.  Use only top two panels in book.
## Colors are scrambled in bottom 6 panels.
A <- ancovaplot(y ~ x + g, data=eco.corr)
B <- ancovaplot(y ~ x, groups=g, data=eco.corr)
update(cbind(superpose=A, ignore.groups=B),
             strip.left=FALSE, between=list(x=1),
             scales=list(alternating=FALSE),
             layout=c(2,1),
             par.settings=list(layout.heights=list(strip=1)))
hhdev.off()


###################################################
### code chunk number 3: grap.tex:226-241
###################################################
hhpdf("grap-pric-bdk.pdf", width=7, height=3.35)
tmp <-
xyplot(sprice ~ beds + drarea + kitarea, data=njgolf, outer=TRUE,
       scales=list(x=list(relation="free"), y=list(alternating=2)),
       layout=c(3,1), between=list(x=1),
       groups=(lotsize==0), pch=c("+","0"), cex=c(2, 1.8), col=col.lot[2:1],
       ylab=NULL, ylab.right="Selling Price",
       xlab=NULL, xlab.top=list("Measures of Dwelling Size", cex=1.2),
       key=list(title="Lot Size", cex.title=1,
         border=TRUE, space="bottom", columns=2,
         points=list(pch=c("0","+"), col=col.lot, cex=c(1.5, 1.8)),
         text=list(c("Zero","Positive"), col=col.lot)))
dimnames(tmp)[[1]] <- c("Beds","Dining Room Area","Kitchen Area")
tmp
hhdev.off()


###################################################
### code chunk number 4: grap.tex:262-284
###################################################
hhpdf("grap-pric-bdkc.pdf", width=7.5, height=5.5)
tmp <-
combineLimits(useOuterStrips(
  update(transpose(
    xyplot(sprice ~ beds + drarea + kitarea |
           factor(lotsizef=="house", labels=c("condominium","house")),
           data=njgolf, outer=TRUE,
           groups=(lotsize==0),
           pch=c("+","0"), cex=c(2, 1.8), col=col.lot[2:1],
           ylab=NULL, ylab.right="Selling Price",
           xlab=NULL, xlab.top=list("Measures of Dwelling Size", cex=1.2),
           layout=c(2,3),
           key=list(title="Lot Size", cex.title=1,
              border=TRUE, space="bottom", columns=2,
              points=list(pch=c("0","+"), col=col.lot, cex=c(1.5, 1.8)),
              text=list(c("Zero","Positive"), col=col.lot)))
           ), between=list(x=1.5, y=1),
           scales=list(x=list(relation="free"), y=list(alternating=2)))
))
dimnames(tmp)[[1]] <- c("Beds","Dining Room Area","Kitchen Area")
tmp
hhdev.off()


###################################################
### code chunk number 5: grap.tex:351-374
###################################################
hhpdf("grap-pbdkcl-color.pdf", width=7, height=7)
tmp <- cbind(njgolf[,c("sprice","lotsize","beds","drarea","kitarea")],
             cond.house=factor(njgolf$lotsizef=="house",
               labels=c("condominium","house")))
tmp.splom <-
splom(~ tmp, axis.text.cex=.5, varname.cex=.8, xlab=NULL,
      group=tmp$cond.house, panel=panel.superpose,
      pch=c("0","+"), cex=c(1.8, 2), col=col.lot[1:2],
      key=list(
        border=TRUE,
        text=list(c("Condominium","House")),
        points=list(pch=c("0","+"), col=col.lot, cex=c(1.5, 1.8)),
        space="bottom", columns=2))
rrows <- c(1,1,1,1)
ccols <- c(2,3,4,5)
cols <- c("gray40","black","black","black")
## when I isolate the contents of the layer() as a function, it doesn't work.
tmp.splom + layer((function(...) {
  if (i %in% rrows && j %in% ccols)
     panel.abline(v=current.panel.limits()$xlim,
                  h=current.panel.limits()$ylim,
                  col=cols[which(i == rrows & j == ccols)], lwd=8)})())
hhdev.off()


###################################################
### code chunk number 6: grap.tex:398-409
###################################################
hhpdf("grap-pbdkc-l.pdf", width=9, height=5)
splom(~ tmp[,1:5] | tmp[,6], par.strip.text=list(cex=1.5),
      axis.text.cex=.5, varname.cex=.8, xlab=NULL,
      groups=tmp$cond.house,
      pch=c("0","+"), cex=c(1.3, 1.5), col=col.lot[1:2],
      key=list(
        border=TRUE,
        text=list(c("Condominium","House")),
        points=list(pch=c("0","+"), col=col.lot, cex=c(1.2, 1.6)),
        space="bottom", columns=2))
hhdev.off()


###################################################
### code chunk number 7: grap.tex:493-496
###################################################
hhcode("array3way.r", '
## This three-way array of scatterplots is constructed in file HHscriptnames("logi.R").
')


###################################################
### code chunk number 8: grap.tex:579-590
###################################################
hhpdf("grap-f1.pdf", width=5.5, height=5.5)
data(tv)
xyplot(male.life.exp ~ fem.life.exp, data=tv,
     main="Life Expectancy", xlab="Female", ylab="Male",
     pch=19, aspect="iso",
     xlim=c(48,90), ylim=c(48,90)) +
  layer(panel.abline(a=0, b=1)) +
  layer(panel.text(x=tv["Japan","fem.life.exp"],
                   y=tv["Japan","male.life.exp"],
                   "(82,76)", pos=4))
hhdev.off()


###################################################
### code chunk number 9: grap.tex:601-644
###################################################
hhpdf("grap-f2.pdf", width=8, height=8)
AA <-
  xyplot(male.life.exp ~ fem.life.exp, data=tv,
         xlim=c(48,85), ylim=c(48,85), aspect="iso",
         panel=panel.text, labels=abbreviate(row.names(tv)),
         cex=.8, col=trellis.par.get("superpose.symbol")$col[1],
         main="a. abbreviated names") +
  layer(panel.abline(a=0, b=1))


BB <-
  xyplot(male.life.exp ~ fem.life.exp, data=tv, pch=19,
         xlim=c(48,85), ylim=c(48,85), aspect="iso",
         main="b. simulated interactive") +
  layer(panel.abline(a=0, b=1)) +
  layer(panel.xyplot(x[2], y[2], cex=1.5, pch=19)) +
  layer(panel.text(x[2]+3, y[2]+3, abbreviate(row.names(tv)[2]), cex=1, pos=2))

CC <-
xyplot(male.life.exp ~ fem.life.exp, data=tv,
     pch=19, aspect=1,
     main="c. square, unequal scale")

DD <-
  xyplot(male.life.exp ~ fem.life.exp, data=tv,
       pch=19, aspect=1,
       main="d. x=y line, square, unequal scale") +
  layer(panel.abline(a=0, b=1))

EE <-
  xyplot(male.life.exp ~ fem.life.exp, data=tv,
         pch=19, aspect=.4,
         xlim=c(50,85), ylim=c(50,85),
         main="e. same xlim and ylim, unequal scale,\nleast squares line") +
  layer(panel.abline(a=0, b=1)) +
  layer(panel.abline(lm(male.life.exp ~ fem.life.exp, data=tv)))

print(position=c(0.0, 0.45, 0.55, 1.00), more=TRUE, AA)
print(position=c(0.0, 0.00, 0.55, 0.45), more=TRUE, BB)
print(position=c(0.5, 0.67, 1.0, 1.00), more=TRUE, CC)
print(position=c(0.5, 0.35, 1.0, 0.68), more=TRUE, DD)
print(position=c(0.5, 0.00, 1.0, 0.35), more=FALSE, EE)
hhdev.off()


###################################################
### code chunk number 10: grap.tex:717-722
###################################################
hhpdf("grap-f3.pdf", width=5.5, height=5.5)
splom( ~ tv[,c(4,5,1,2,3)],
      main=list("Televisions, Physicians, and Life Expectancy", cex=1.4),
      axis.text.cex=.5, pch=19, xlab=NULL, cex=.7)
hhdev.off()


###################################################
### code chunk number 11: grap.tex:805-808
###################################################
hhpdf("grap-f11a.pdf", width=7.5, height=6.5)
pairs(tv, pch=19, main="pairs with NW--SE diagonal and rectangular panels")
hhdev.off()


###################################################
### code chunk number 12: grap.tex:836-874
###################################################
hhpdf("grap-f12.pdf", width=9, height=5)
f12b <- matrix(c("Var1",1,2,4,
                 "1'","Var2",3,5,
                 "2'","3'","Var3",6,
                 "4'","5'","6'","Var4"), 4, 4)
dd <- c(1,6,11,16)

par(mfrow=c(1,2))
old.par <- par(pty="s", usr=c(.5, 4.5,  .5, 4.5), mar=par("mar")+c(0,1,0,0))

plot(x=c(.5, 4.5), y=c(.5, 4.5), type="n", las=1, yaxt="n",
     cex=1.2,
     main="a. multiple axes of symmetry.",
     xlab="variables", ylab="variables")
axis(2, at=1:4, labels=rep("",4), cex=1.2)
     axis(2, at=1:4, labels=4:1, cex=1.2, las=1)
text(f12b[-dd], y=5-row(f12b)[-dd], x=col(f12b)[-dd], cex=2)
text(f12b[ dd], y=5-row(f12b)[ dd], x=col(f12b)[ dd], cex=1.4)
abline(a=5, b=-1)
abline(a=-2, b=1, lty=3, lwd=3)
abline(a=-1, b=1, lty=2)
abline(a= 0, b=1, lty=2)
abline(a= 1, b=1, lty=2)
abline(a= 2, b=1, lty=2)
  arrows(2.8, .75, 2.8,1.14, length=.1) ## up arrow
  arrows(3.8,1.7, 4.15,1.7, length=.1)  ## right arrow

plot(x=c(.5,4.5), y=c(.5,4.5), type="n", las=1,
     cex=1.2,
     main="b. single axis of symmetry.",
     xlab="variables", ylab="variables")
text(f12b[-dd], y=row(f12b)[-dd], x=col(f12b)[-dd], cex=2)
text(f12b[ dd], y=row(f12b)[ dd], x=col(f12b)[ dd], cex=1.4)
abline(a=0,b=1)
  arrows(2.8,3.75, 2.8,4.14, length=.1) ## up arrow
  arrows(3.8,2.7, 4.15,2.7, length=.1)  ## right arrow
par(old.par)
hhdev.off()


###################################################
### code chunk number 13: grap.tex:953-958
###################################################
hhpdf("grap-f5.pdf", width=6.5, height=6.5)
splom( ~ tv[, 1:3],
      main=list("Televisions, Physicians, and Life Expectancy", cex=1.4),
      xlab=NULL, pch=19, cex=.9, varname.cex=1.3)
hhdev.off()


###################################################
### code chunk number 14: grap.tex:994-999
###################################################
hhpdf("grap-f6.pdf", width=6.5, height=6.5)
splom( ~ cbind(tv[,1,drop=FALSE], log(tv[, 2:3])),
      main=list("log(Televisions, Physicians), and Life Expectancy", cex=1.4),
      xlab=NULL, pch=19, cex=.9, varname.cex=1.3)
hhdev.off()


###################################################
### code chunk number 15: grap.tex:1075-1115
###################################################
hhpdf("grap-f8.pdf", width=10.3, height=4.5)
dy2 <- format(c(-1, -0.5, 0, 0.5, 1, 2))

x <- seq(0, 2, length=101)
y <- data.matrix(ladder.f(x+.001))

col.power <- colorspace:::rainbow_hcl(6, c=100)

par(fig=c( .025,.325,0,1)      )  ## left 1/3
matplot(x=x, y=y, type="l", lty=1, lwd=3, col=col.power,
        xlim=c(0,2), ylim=c(-2,4), err=-1,
        main=list("a. Simple Powers\nwith Negative Reciprocals\n(monotonic, wrong order)", cex=1.2),
        ylab="")
text(x[101]+.1, y[101,]+c(.2,-.1,0,0,0,-.5), dy2, adj=0,
     xpd=TRUE, col=col.power)
title(ylab=expression(over(x^p, scriptstyle(sign(p)))), xpd=NA)
text(x=2.3, y=-1.5, "p", xpd=NA)

y[,1:2] <- -y[,1:2]
par(fig=c(.345,.645,0,1), new=TRUE)  ## middle 1/3
matplot(x=x, y=y, type="l", lty=1, lwd=3, col=col.power,
        xlim=c(0,2), ylim=c(-2,4), err=-1,
        main=list("b. Simple Powers\nwith Positive Reciprocals\n(not monotonic, wrong order)", cex=1.2),
        ylab="")
text(x[101]+.1, y[101,]+c(-.2,.3,0,0,0,-.5), dy2, adj=0,
     xpd=TRUE, col=col.power)
title(ylab=expression(x^p, xpd=NA))
text(x=2.3, y=-1.5, "p", xpd=NA)

yy <- data.matrix(ladder.fstar(x+.001))
par(fig=c(.700,1.000,0,1), new=TRUE)  ## right 1/3
matplot(x=x, y=yy, type="l", lty=1, lwd=3, col=col.power,
        xlim=c(0,2), ylim=c(-2,2), err=-1,
        main=list("c. Scaled Powers\n(monotonic\nand right order)", cex=1.2),
        ylab="")
text(x[101]+.1, yy[101,]+c(-.21,-.12,0,0.1,0.3,0.1), dy2, adj=0,
     xpd=TRUE, col=col.power)
title(ylab=expression(over(x^p - 1, scriptstyle(p))), xpd=NA)
text(x=2.3, y=-2+1/3, "p", xpd=NA)
hhdev.off()


###################################################
### code chunk number 16: grap.tex:1213-1231
###################################################
hhpdf("grap-f7.pdf", width=7, height=7)
data(tv)
le <- ladder.fstar(tv$life.exp, "LE^")
ppp <- ladder.fstar(tv$ppl.per.phys, "PPP^")

form <-
  as.formula(paste(paste("`", names(le),  "`", sep="", collapse="+"), "~",
                   paste("`", names(ppp), "`", sep="", collapse="+")))

tmp <- xyplot(form, data=cbind(le, ppp), outer=TRUE, layout=c(6,6),
              pch=19, cex=.7, scales="free",
              between=list(x=.5, y=.5), aspect=1,
              xlab=NULL, xlab.top="People per Physician\n",
              ylab="Life Expectancy")
tmp2 <- useOuterStrips(combineLimits(matrix.trellis(tmp, byrow=TRUE, nrow=6, ncol=6)))
dimnames(tmp2) <- list(names(ppp), names(le))
update(tmp2, scales=list(at=NULL, labels=NULL))
hhdev.off()


###################################################
### code chunk number 17: grap.tex:1254-1263
###################################################
hhpdf("grap-f9.pdf", width=7, height=4)
x <- sort(tv[,"life.exp"])
y <- ladder.fstar(x)
form <- as.formula(paste(paste("`", names(y), "`", sep="", collapse="+"), "~ x"))
xyplot(form, data=cbind(x=x, y), outer=TRUE, layout=c(3,2),
       strip=strip.custom(factor.levels=paste("x^", names(y), sep="")),
       type="l", scales=list(alternating=1, y="free", rot=0),
       between=list(y=1), ylab=NULL)
hhdev.off()


###################################################
### code chunk number 18: grap.tex:1310-1331
###################################################
y <- ladder.fstar(tv$ppl.per.phys)
g <- paste("ppp ^",names(y))
g <- ordered(g, g)
g <- g[col(y)]
gg <- y
gg[] <- ''
hhpdf("grap-f10bw.pdf", width=7, height=3)
tmpbw <-
bwplot(unlist(y) ~ unlist(gg) | g, groups=g,
       layout=c(6,1), xlab=NULL, ylab=NULL,
       scales="free", panel=panel.bwplot.superpose)
update(tmpbw, scales=list(at=NULL, labels=NULL))
hhdev.off()
##
hhpdf("grap-f10sp.pdf", width=7, height=3)
tmpsp <-
stripplot(unlist(y) ~ unlist(gg) | g, groups=g,
          layout=c(6,1), xlab=NULL, ylab=NULL, pch=19,
          scales=list(relation="free", cex=.8))
update(tmpsp, scales=list(at=NULL, labels=NULL))
hhdev.off()


###################################################
### code chunk number 19: grap.tex:1350-1362
###################################################
scales <- c(2, 3, 3, 2, 2, 2)
names(scales) <- names(y)
for (i in names(y)) assign(i, y[[i]])
##
for (i in names(y)) {
  i.Rout <- paste("grap-f10-", i, ".Rout", sep="")
##  i.stem <- paste("stem(-y$`", i, "`, scale=", scales[i],")", sep="")
  i.stem <- paste("stem(-`", i, "`, scale=", scales[i],")", sep="")
  hhcapture(i.Rout, i.stem)
}
## I manually took these six files and edited them.
## The edited file is included as part c of the figure.


