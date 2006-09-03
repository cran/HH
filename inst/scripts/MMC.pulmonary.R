## This example is from Hsu and Peruggia
##
## Hsu, J. and Peruggia, M. (1994).
## "Graphical representations of {Tukey's} multiple comparison method."
## Journal of Computational and Graphical Statistics, 3:143--161.

pulmonary <- read.table(hh("datasets/pulmonary.dat"), header=TRUE,
                        row.names="group")
pulmonary

anova.mean(row.names(pulmonary),
           pulmonary$n,
           pulmonary$ybar,
           pulmonary$s,
           ylabel="pulmonary")


## simint or multicomp object
pulmonary.mca <-
if.R(r=
simint.mean(row.names(pulmonary),
            pulmonary$n,
            pulmonary$ybar,
            pulmonary$s,
            ylabel="pulmonary",
            focus="smoker")
,s=
multicomp.mean(row.names(pulmonary),
               pulmonary$n,
               pulmonary$ybar,
               pulmonary$s,
               ylabel="pulmonary",
               focus="smoker")
)

pulmonary.mca
## lexicographic ordering of contrasts, some positive and some negative
plot(pulmonary.mca)



pulm.lmat <- cbind("npnl-mh"=c( 1, 1, 1, 1,-2,-2), ## not.much vs lots
                   "n-pnl"  =c( 3,-1,-1,-1, 0, 0), ## none vs light 
                   "p-nl"   =c( 0, 2,-1,-1, 0, 0), ## {} arbitrary 2 df
                   "n-l"    =c( 0, 0, 1,-1, 0, 0), ## {} for 3 types of light
                   "m-h"    =c( 0, 0, 0, 0, 1,-1)) ## moderate vs heavy
dimnames(pulm.lmat)[[1]] <- row.names(pulmonary)
pulm.lmat


## mmc.multicomp object
pulmonary.mmc <-
if.R(r=
simint.mmc.mean(row.names(pulmonary),
                pulmonary$n,
                pulmonary$ybar,
                pulmonary$s,
                ylabel="pulmonary",
                focus="smoker",
                lmat=pulm.lmat)
,s=
multicomp.mmc.mean(row.names(pulmonary),
                   pulmonary$n,
                   pulmonary$ybar,
                   pulmonary$s,
                   ylabel="pulmonary",
                   focus="smoker",
                   lmat=pulm.lmat,
                   plot=FALSE)
)


gray <- if.R(r="gray", s=16)
red  <- if.R(r="red",  s=8)
blue <- if.R(r="blue", s=6)

old.par <- if.R(s=par(mar=c(5,4,4,4)+.1),
                r=par(mar=c(15,4,4,4)+.1))

## pairwise comparisons
## MMC Figure 7a
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=FALSE,
     col.mca.signif=red, col.iso=16)

## tiebreaker plot, with contrasts ordered to match MMC plot,
## with all contrasts forced positive, and with names also reversed.
if.R(r={
pulmonary.xlim <- par()$usr[1:2]
plot(pulmonary.mmc$mca, xlim=pulmonary.xlim, xaxs="i", main="", xlab="")
},s={
plot(pulmonary.mmc$mca, col.signif=red, lty.signif=1, xlabel.print=FALSE,
     xaxs="d",  plt=par()$plt+c(0,0,-.25,.05), xrange.include=c(-1, 1))
})

## orthogonal contrasts
## MMC Figure 7b
plot(pulmonary.mmc, print.lmat=TRUE, col.lmat.signif=blue, col.iso=16)

## pairwise and orthogonal contrasts on the same plot
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=TRUE,
     col.mca.signif=red, col.lmat.signif=blue, col.iso=16,
     lty.lmat.not.signif=2)

par(old.par)
