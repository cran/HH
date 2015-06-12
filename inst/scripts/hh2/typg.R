### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/typg.tex'

###################################################
### code chunk number 1: typg.tex:9-10
###################################################
library(HH)


###################################################
### code chunk number 2: typg.tex:407-429
###################################################
## hhcapture("dump.Rout", '
tmp <- data.frame(aa=1:3, bb=factor(4:6), cc=letters[7:9],
                  dd=factor(LETTERS[10:12]), stringsAsFactors=FALSE)
str(tmp)
tmp
dump("tmp", "")

tmp <- read.table(text="
  aa bb cc dd
1  1  4  g  J
2  2  5  h  K
3  3  6  i  L
", header=TRUE)
sapply(tmp, class)
tmp <-
structure(list(aa = 1:3, bb = structure(1:3, .Label = c("4",
"5", "6"), class = "factor"), cc = c("g", "h", "i"),
dd = structure(1:3, .Label = c("J",
"K", "L"), class = "factor")), .Names = c("aa", "bb", "cc", "dd"
), row.names = c(NA, -3L), class = "data.frame")
sapply(tmp, class)
## ')


###################################################
### code chunk number 3: typg.tex:488-502
###################################################
hhcode("extract.r", '
mydata <- data.frame(x=1:6, y=c(1,4,2,3,6,2))

my.lm <- lm( y ~ x , data=mydata)

summary(my.lm) ## summary() method on lm argument
old.mfrow <- par(mfrow=c(2,2)) ## four panels on the graphics device
plot(my.lm)    ## plot() method on lm argument
par(old.mfrow)                 ## restore previous arrangement
coef(my.lm)    ## coef() method on lm argument
anova(my.lm)   ## anova() method on lm argument
resid(my.lm)   ## resid() method on lm argument
predict(my.lm) ## predict() method on lm argument
## ')


###################################################
### code chunk number 4: typg.tex:580-586
###################################################
tmp <- data.frame(y=rnorm(10), x=1:10)
tmp.lm <- lm(y ~ x, data=tmp)
anova(tmp.lm)
## hhpdf("notbigresid.pdf", height=4.5, width=4.5)
plot(tmp.lm, 2)
## hhdev.off()


