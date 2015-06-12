### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/RApx.tex'

###################################################
### code chunk number 1: RApx.tex:9-10
###################################################
library(HH)


###################################################
### code chunk number 2: RApx.tex:49-65
###################################################
hhcode("install.packagesLM.R", '
install.packages(c("HH","RcmdrPlugin.HH","RcmdrPlugin.mosaic",
                   "fortunes","ggplot2","shiny","gridExtra",
                   "gridBase","Rmpfr","png","XLConnect",
                   "matrixcalc", "sem", "relimp", "lmtest",
                   "markdown", "knitr", "effects", "aplpack",
                   "RODBC", "TeachingDemos"),
                 dependencies=TRUE)

## This is the sufficient list (as of 07 June 2015) of packages
## needed in order to install the HH package.  Should
## additional dependencies be declared by any of these packages
## after that date, the first use of "library(HH)" after the
## installation might ask for permission to install some more
## packages.
## ')


###################################################
### code chunk number 3: RApx.tex:96-101
###################################################
hhcode("install.packages.R", '
## Tell Windows that R should have the same access to the
## outside internet that is granted to Internet Explorer.
setInternet2()
## ')


###################################################
### code chunk number 4: RApx.tex:103-107
###################################################
hhcode("install.RcmdrW.R", '
install.packages("Rcmdr",
                 dependencies=TRUE)
## ')


###################################################
### code chunk number 5: RApx.tex:269-274
###################################################
## hhcapture("simpleR.Rout", '
## Simple R session
3 + 4
pnorm(c(-1.96, -1.645, -0.6745, 0, 0.6745, 1.645, 1.96))
## ')


###################################################
### code chunk number 6: RApx.tex:302-305
###################################################
hhcode("manual.R", '
system.file("../../doc/manual")
## ')


###################################################
### code chunk number 7: RApx.tex:307-310
###################################################
hhcode("manualWindows.R", '
WindowsPath(system.file("../../doc/manual"))
## ')


###################################################
### code chunk number 8: RApx.tex:359-362
###################################################
hhcode("HHscript2.R", '
HHscriptnames()
## ')


###################################################
### code chunk number 9: RApx.tex:364-367
###################################################
hhcode("HHscript1.R", '
HHscriptnames(edition=1)
## ')


###################################################
### code chunk number 10: RApx.tex:369-372
###################################################
hhcode("HHscriptW2.R", '
WindowsPath(HHscriptnames())
## ')


###################################################
### code chunk number 11: RApx.tex:374-377
###################################################
hhcode("HHscriptW1.R", '
WindowsPath(HHscriptnames(edition=1))
## ')


