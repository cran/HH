### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/rega.tex'

###################################################
### code chunk number 1: rega.tex:111-115
###################################################
hhpdf("f1.pdf", height=7, width=7)
data(fat)
splom( ~ fat, main="Fat data", pch=19, xlab=NULL)
hhdev.off()


###################################################
### code chunk number 2: rega.tex:182-215
###################################################
hhpdf("resid2x2.pdf", height=5, width=7)
A <- regrresidplot(fat$abdomin, fat$bodyfat, fit.line=FALSE,
                   xlim=c(70,185), ylim=c(0,50))

B <- regrresidplot(fat$abdomin, fat$bodyfat,
                   xlim=c(70,185), ylim=c(0,50), resid.plot="line")

C <- regrresidplot(fat$abdomin, fat$bodyfat,
                   xlim=c(70,185), ylim=c(0,50), resid.plot="square")

fat$shallow <- 20 + .1*fat$abdomin
fat.shallow.lm <- lm(shallow ~ abdomin, data=fat)

D <- regrresidplot(fat$abdomin, fat$bodyfat,
                   xlim=c(70,185), ylim=c(0,50), resid.plot="line",
                   lm.object=fat.shallow.lm)

E <- regrresidplot(fat$abdomin, fat$bodyfat,
                   xlim=c(70,185), ylim=c(0,50), resid.plot="square",
                   lm.object=fat.shallow.lm)

ALL <-
update(c(C, E, A, B, D, layout=c(3,2)),
       skip=c(TRUE, rep(FALSE, 5)),
       scales=list(alternating=FALSE),
       between=list(x=2, y=2),
       xlab="abdomin", ylab="bodyfat",
       xlab.top=c("\n",
         "least-squares fit:\ny = -28.56 + .505 x",
         "too shallow:\ny = 20 + .1x"),
       ylab.right=list(c("squared\nresiduals","residuals"), rot=0))
ALL
hhdev.off()


###################################################
### code chunk number 3: rega.tex:330-335
###################################################
hhcapture("bodyfatlm.Rout", '
fat.lm <- lm(bodyfat ~ abdomin, data=fat)
anova(fat.lm)
summary(fat.lm, corr=FALSE)
')


###################################################
### code chunk number 4: rega.tex:609-622
###################################################
hhpdf("f5.pdf", height=5, width=8)
B <- regrresidplot(fat$abdomin, fat$bodyfat,
                   ylim=c(0,50), resid.plot="line")

F <- regrresidplot(fat$abdomin, fat$bodyfat,
                   ylim=c(0,50), resid.plot="line",
                   lm.object=lm(bodyfat ~ 1, data=fat))

update(c(F, B, layout=c(2,1)), between=list(x=1),
       xlab="abdomin", ylab="bodyfat",
       scales=list(alternating=FALSE),
       xlab.top=c("Variance about\nmean of y", "Variance about\nleast-squares line"))
hhdev.off()


###################################################
### code chunk number 5: rega.tex:717-723
###################################################
hhpdf("betaWeightedAverage.pdf", height=5, width=7)
demo(betaWeightedAverage, ask=FALSE, package="HH")
hhdev.off()
hhcapture("betaWeightedAverage.Rout", '
bWA
')


###################################################
### code chunk number 6: rega.tex:795-821
###################################################
hhcapture("meansquare.Rout", '
h <- hat(model.matrix(fat.lm))
pred <- predict(fat.lm, se.fit=TRUE)
res <- resid(fat.lm)
sigma.hat.square <- anova(fat.lm)["Residuals", "Mean Sq"]
fat.predvalues <-
data.frame("y=bodyfat"=fat$bodyfat,  "x=abdomin"=fat$abdomin,
           h=h,                      mu.hat=pred$fit,
           e=res,                    var.mu.hat=h*sigma.hat.square,
           var.resid=(1-h)*sigma.hat.square,
           sigma.hat.square=sigma.hat.square,
           se.fit=sqrt(h*sigma.hat.square),
           se.resid=sqrt((1-h)*sigma.hat.square))
fat.predvalues[1:3, 1:7]
## fat.predvalues

## linear identity
all.equal(rowSums(fat.predvalues[,c("mu.hat", "e")]),
          fat$bodyfat,
          check.names=FALSE)
## quadratic identity
(SSqReg <- sum((fat.predvalues$mu.hat - mean(fat$bodyfat))^2))
(SSqRes <- sum(res^2))
(SSqTot <- sum((fat$bodyfat - mean(fat$bodyfat))^2))
all.equal(SSqReg + SSqRes, SSqTot)
')


###################################################
### code chunk number 7: rega.tex:874-880
###################################################
hhcapture("fatpredvalue.Rout", '
cbind(fat.predvalues[, 1],
      ybar=18.4,
      round(coef(fat.lm)[2]*(fat.predvalues[,2]-mean(fat.predvalues[,2])), 3),
      round(fat.predvalues[, 5], 3))
')


###################################################
### code chunk number 8: rega.tex:1110-1120
###################################################
old.data <- data.frame(y=rnorm(50), x1=rnorm(50), x2=rnorm(50), x3=rnorm(50))
example.lm <- lm(y ~ x1 + x2 + x3, data=old.data)
predict(example.lm,
        newdata=data.frame(x1=3, x2=2, x3=45),
        se.fit=TRUE,
        interval="confidence")
predict(example.lm,
        newdata=data.frame(x1=3, x2=2, x3=45),
        se.fit=TRUE,
        interval="prediction")


###################################################
### code chunk number 9: rega.tex:1301-1304
###################################################
hhpdf("fat-ci.pdf", height=6, width=7)
ci.plot(fat.lm, xlab=list(cex=1.4), ylab=list(cex=1.4), main.cex=1.4, aspect=1)
hhdev.off()


###################################################
### code chunk number 10: rega.tex:1359-1362
###################################################
hhpdf("f6.pdf", height=7, width=9)
lmplot(fat.lm)
hhdev.off()


###################################################
### code chunk number 11: rega.tex:1528-1581
###################################################
hhpdf("diag1.pdf", height=6, width=4)
pp <- ppoints(101)
x <- qnorm(pp)
xyplot(pp ~ x, main="Cumulative Distribution of N(0,1)")
hhdev.off()

hhpdf("diag2.pdf", height=6, width=7)
n <- nrow(fat)
f <- (1:n)/n
diag2a <- xyplot(f ~ sort(predict(fat.lm)),
                 main="empirical cdf of\nfitted values")
diag2b <- xyplot(f ~ sort(resid(fat.lm)),
                 main="empirical cdf of\nresiduals")
## export.eps(hh.file("rega/figure/diag2.eps"))
update(c(diag2a, diag2b, layout=c(2,1)),
       xlab=c(diag2a$xlab, diag2b$xlab),
       xlab.top=c(diag2a$main, diag2b$main),
       main=NULL,
       scales=list(x=list(alternating=FALSE)),
       between=list(x=1))
hhdev.off()

hhpdf("diag3.pdf", height=6, width=7)
diag3a <- xyplot(f ~ sort(predict(fat.lm) - mean(predict(fat.lm))),
                 main="empirical cdf of\ncentered fitted values")
diag3b <- xyplot(f ~ sort(resid(fat.lm)),
                 main="empirical cdf of\n residuals")
## export.eps(hh.file("rega/figure/diag3.eps"))
tmp3 <- update(c(diag3a, diag3b, layout=c(2,1)),
               xlab=c(diag3a$xlab, diag3b$xlab),
               xlab.top=c(diag3a$main, diag3b$main),
               main=NULL,
               scales=list(x=list(alternating=FALSE)),
               between=list(x=1))
tmp3c <-
combineLimits(as.matrix(row=TRUE, tmp3), margin.x=1)
tmp3c
hhdev.off()

hhpdf("diag4.pdf", height=6, width=7)
diag4a <- xyplot(sort(predict(fat.lm) - mean(predict(fat.lm))) ~ f,
                 main="transposed empirical cdf\nof centered fitted values")
diag4b <- xyplot(sort(resid(fat.lm)) ~ f,
                 main="transposed empirical\ncdf of residuals")
## export.eps(hh.file("rega/figure/diag4.eps"))
tmp4 <- update(c(diag4a, diag4b, layout=c(2,1)),
               xlab.top=c(diag4a$main, diag4b$main),
               main=NULL,
               ylab.right=diag4b$ylab,
               scales=list(x=list(alternating=FALSE)),
               between=list(x=1))
combineLimits(as.matrix(row=TRUE, tmp4))
hhdev.off()


###################################################
### code chunk number 12: rega.tex:1645-1689
###################################################
hhpdf("regrFitRes.pdf", height=7, width=9)
x <- rnorm(100)
e <- rnorm(100)

y1 <- sqrt(.1) * x + sqrt(1-.1) * e
y5 <- sqrt(.5) * x + sqrt(1-.5) * e
y9 <- sqrt(.9) * x + sqrt(1-.9) * e

rr <- data.frame(x, e, y1, y5, y9)
ThreeR2 <-
c(
diagplot5new(lm(y1 ~ x, data=rr), ylim=c(-3.2, 3.2)),
diagplot5new(lm(y5 ~ x, data=rr), ylim=c(-3.2, 3.2)),
diagplot5new(lm(y9 ~ x, data=rr), ylim=c(-3.2, 3.2))
)
TM <- update(matrix.trellis(ThreeR2, byrow=TRUE, nrow=3, ncol=2), between=list(y=1))
## TM

TMn <- update(TM,
              xlab.top=c(expression(hat(y)-bar(y)), expression(y-hat(y))),
              ylab="", ylab.right=NULL)
## TMn

print(position=c(0, 0, .4, 1), more=TRUE,
      update(strip=FALSE, xlab.top=expression(phantom(hat(y))*y*phantom(hat(y))),
             ylab=NULL,
             ylab.right=list(c(
               expression(R^2==.1),
               expression(R^2==.5),
               expression(R^2==.9)), rot=0),
             ylim=c(-3.2, 3.2),
        xyplot(y1 + y5 + y9 ~ x, data=rr, outer=TRUE,
               panel=function(...) {
                  panel.lmline(...)
                  panel.points(...)
               },
               layout=c(1,3),
               scales=list(alternating=FALSE), between=list(y=1))
))

print(position=c(.39, 0, 1, 1), more=FALSE,
update(TMn, ylim=c(-3.2, 3.2))
)
hhdev.off()


