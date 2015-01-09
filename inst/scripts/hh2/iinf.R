### R code from vignette source '~/WindowsC/HOME/rmh/hh.e2/hh2/iinf.tex'

###################################################
### code chunk number 1: iinf.tex:118-143
###################################################
hhpdf("Hyz6.pdf", height=8.5, width=7)

NTfun <- function(...)
   NTplot(..., main=NULL, xlab=NULL, ylab=NULL, prob.labels=FALSE,
          xhalf.multiplier=1.6, yhalf.multiplier=2, cex.top.axis=.7,
          scales=list(cex=.5, y=list(at=1)),
          xlim=c(-3, 4))

Nhr  <- NTfun(mean0=0, xbar= 1.8, alpha.right=.05,  alpha.left=0)
Nhl  <- NTfun(mean0=0, xbar=-1.8, alpha.right=0,    alpha.left=.05)
Nhlr <- NTfun(mean0=0, xbar= 1.8, alpha.right=.025, alpha.left=.025)

Ncl  <- NTfun(mean0=0, xbar= 1.8, alpha.right=0,    alpha.left=.05,  type="confidence")
Ncr  <- NTfun(mean0=0, xbar=-1.8, alpha.right=.05,  alpha.left=0,    type="confidence")
Nclr <- NTfun(mean0=0, xbar= 1.8, alpha.right=.025, alpha.left=.025, type="confidence")

print(Nhr,  tablesOnPlot=FALSE, position=c(.00, .630, .55, 1.000), more=TRUE)
print(Nhl,  tablesOnPlot=FALSE, position=c(.00, .315, .55,  .685), more=TRUE)
print(Nhlr, tablesOnPlot=FALSE, position=c(.00, .000, .55,  .370), more=TRUE)

print(Ncl,  tablesOnPlot=FALSE, position=c(.45, .630, 1.0, 1.000), more=TRUE)
print(Ncr,  tablesOnPlot=FALSE, position=c(.45, .315, 1.0,  .685), more=TRUE)
print(Nclr, tablesOnPlot=FALSE, position=c(.45, .000, 1.0,  .370), more=FALSE)

hhdev.off()


