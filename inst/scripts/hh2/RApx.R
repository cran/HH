### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/RApx.tex'

###################################################
### code chunk number 1: RApx.tex:43-49
###################################################
hhcode("install.packages.R", '
install.packages(c("HH","RcmdrPlugin.HH","RcmdrPlugin.mosaic",
                   "fortunes","ggplot2","shiny","gridExtra",
                   "gridBase","Rmpfr","png"),
                 dependencies=TRUE)
')


###################################################
### code chunk number 2: RApx.tex:100-105
###################################################
hhcapture("simpleR.Rout", '
## Simple R session
3 + 4
pnorm(c(-1.96, -1.645, -0.6745, 0, 0.6745, 1.645, 1.96))
')


###################################################
### code chunk number 3: RApx.tex:131-134
###################################################
hhcode("manual.R", '
system.file("../../doc/manual")
')


###################################################
### code chunk number 4: RApx.tex:136-139
###################################################
hhcode("manualWindows.R", '
WindowsPath(system.file("../../doc/manual"))
')


###################################################
### code chunk number 5: RApx.tex:184-187
###################################################
hhcode("HHscript2.R", '
HHscriptnames()
')


###################################################
### code chunk number 6: RApx.tex:189-192
###################################################
hhcode("HHscript1.R", '
HHscriptnames(edition=1)
')


###################################################
### code chunk number 7: RApx.tex:194-197
###################################################
hhcode("HHscriptW2.R", '
WindowsPath(HHscriptnames())
')


###################################################
### code chunk number 8: RApx.tex:199-202
###################################################
hhcode("HHscriptW1.R", '
WindowsPath(HHscriptnames(edition=1))
')


