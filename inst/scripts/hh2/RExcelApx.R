### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/RExcelApx.tex'

###################################################
### code chunk number 1: RExcelApx.tex:55-62
###################################################
hhcode("install.packages.RExcel.R", '
install.packages(c("rscproxy","rcom"),
                 repos="http://rcom.univie.ac.at/download",
                 lib=.Library)
library(rcom)
comRegisterRegistry()
')


