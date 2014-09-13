### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/RApx.tex'

###################################################
### code chunk number 1: RApx.tex:92-97
###################################################
hhcapture("simpleR.Rout", '
## Simple R session
3 + 4
pnorm(c(-1.96, -1.645, -0.6745, 0, 0.6745, 1.645, 1.96))
')


###################################################
### code chunk number 2: RApx.tex:156-165
###################################################
## run this on Windows!
hhcapture("gsubslash.Rout", '
system.file("scripts", package="HH")
## Yes, 4 "\" characters
gsub("/", "\\\\", system.file("scripts", package="HH"))
## The above displays as double-backslashes.
## There is really only one, which you can see with cat().
cat(gsub("/", "\\\\", system.file("scripts", package="HH")), "\n")
')


