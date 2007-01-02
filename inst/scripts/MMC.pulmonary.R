## This example is from Hsu and Peruggia
##
## Hsu, J. and Peruggia, M. (1994).
## "Graphical representations of {Tukey's} multiple comparison method."
## Journal of Computational and Graphical Statistics, 3:143--161.

pulmonary <- read.table(hh("datasets/pulmonary.dat"), header=TRUE,
                        row.names=NULL)
names(pulmonary)[3] <- "FVC"
names(pulmonary)[1] <- "smoker"
pulmonary$smoker <- factor(pulmonary$smoker, levels=pulmonary$smoker)
row.names(pulmonary) <- pulmonary$smoker
pulmonary
pulmonary.aov <- aov.sufficient(FVC ~ smoker,
                                data=pulmonary,
                                weights=pulmonary$n,
                                sd=pulmonary$s)
summary(pulmonary.aov)


## glht or multicomp object
pulmonary.mca <-
if.R(r=
     glht(pulmonary.aov,
          linfct=mcp(smoker="Tukey"),
          df=pulmonary.aov$df.residual,
          vcov.=vcov.sufficient)
,s=
multicomp.mean(pulmonary$smoker,
               pulmonary$n,
               pulmonary$FVC,
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
if.R(r=pulm.lmat <- rbind(Int=0, pulm.lmat[-1,]),
     s={})
pulm.lmat

## mmc.multicomp object
pulmonary.mmc <-
  if.R(r=
       glht.mmc(pulmonary.aov,
                linfct=mcp(smoker="Tukey"),
                df=pulmonary.aov$df.residual,
                vcov.=vcov.sufficient,
                lmat=pulm.lmat,
                calpha=attr(confint(pulmonary.mca)$confint,"calpha"))
       ,s=
       multicomp.mmc.mean(pulmonary$smoker,
                          pulmonary$n,
                          pulmonary$FVC,
                          pulmonary$s,
                          ylabel="pulmonary",
                          focus="smoker",
                          lmat=pulm.lmat,
                          plot=FALSE)
       )


old.par <- if.R(s=par(mar=c(5,4,4,4)+.1),
                r=par(mar=c(15,4,4,4)+.1))

## pairwise comparisons
## MMC Figure 7a
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=FALSE)

## tiebreaker plot, with contrasts ordered to match MMC plot,
## with all contrasts forced positive and with names also reversed,
## and with matched x-scale.
if.R(r=
plot(confint(as.glht(pulmonary.mmc$mca)),
     xlim=par()$usr[1:2], xaxs="i", main="", xlab="")
,s=
plot(pulmonary.mmc$mca, col.signif='red', lty.signif=1, xlabel.print=FALSE,
     xaxs="d",  plt=par()$plt+c(0,0,-.25,.05), xrange.include=c(-1, 1))
)

## orthogonal contrasts
## MMC Figure 7b
plot(pulmonary.mmc, print.lmat=TRUE, col.lmat.signif='blue', col.iso='gray')

## pairwise and orthogonal contrasts on the same plot
plot(pulmonary.mmc, print.mca=TRUE, print.lmat=TRUE,
     col.mca.signif='red', col.lmat.signif='blue', col.iso='gray',
     lty.lmat.not.signif=2)

par(old.par)
