## These data reprinted in \cite{hand:1994} are originally from Pearce,
## S.C., 1983, The Agricultural Field Experiment, Wiley.
##
## The response is crop yield in pounds and the covariable is yield
## in bushels in a prior period under the same growing conditions.
## The treatments are growing conditions, where level 6 is a control.
## There are 4 blocks.  Hand implies that treat is significant iff
## the covariable is taken into account.
##

## Please see files hh/dsgntwo/code/apple3.s and
## hh/dsgntwo/code/apple3.r for the discussion of the apple example in
## HH.  This file contains only the minimum needed to reproduce the
## example in the MMC paper:
##   Heiberger, Richard M. and Holland, Burt (2006). "Mean--mean
##   multiple comparison displays for families of linear contrasts."
##   Journal of Computational and Graphical Statistics, 15:937--955.



data(apple)

apple.ancova.2 <- aov(yield ~ block + pre + treat, data=apple, x=TRUE)
anova(apple.ancova.2)
apple.ancova.2$x
coef(apple.ancova.2)
predict(apple.ancova.2)


## find and remove block effect from response variable and covariable
yield.block.effect <- fitted(lm(yield ~ block, data=apple))-mean(apple$yield)
pre.block.effect   <- fitted(lm(pre   ~ block, data=apple))-mean(apple$pre)
yield.block        <- apple$yield-yield.block.effect
pre.block          <- apple$pre-pre.block.effect
apple <- cbind(apple, yield.block=yield.block, pre.block=pre.block)

## Same sums of squares as apple.ancova.2
## for pre and treat adjusted for block
## The residual sum of squares is correct.
## The residual Df includes the block df and is therefore wrong.
apple.ancova.4 <- ancova(yield.block ~ pre.block + treat, data=apple)
anova(apple.ancova.4)

yield.block.pre <-
  apple$yield.block -
  predict.lm(apple.ancova.4, type="terms", terms="pre.block")

apple <- cbind(apple, yield.block.pre=as.vector(yield.block.pre))
apple.ancova.5 <- ancova(yield.block.pre ~ treat, x=pre.block, data=apple)
anova(apple.ancova.5)
if.R(s=attr(apple.ancova.5, "trellis")$ylim <-
     attr(apple.ancova.4, "trellis")$ylim,
     r=attr(apple.ancova.5, "trellis")$y.limits <-
     attr(apple.ancova.4, "trellis")$y.limits)


## apple.ancova.2 and apple.ancova.4 have the same Sums of Squares in
## the anova table and the same regression coefficients.
summary.lm(apple.ancova.2, corr=FALSE)
summary.lm(apple.ancova.4, corr=FALSE)
summary.lm(apple.ancova.5, corr=FALSE)
summary(apple.ancova.2)
summary(apple.ancova.4)
summary(apple.ancova.5)
## apple.ancova.2 has the correct residual df, hence Mean Squares and F tests.
## apple.ancova.4 has the wrong   residual df, hence Mean Square and F tests.
## apple.ancova.5 has the wrong   residual df, hence Mean Square and F tests,
##                and the wrong   treat Sum of Squares.  It has the correct
##                regression coefficients.
 
## MMC Figure 6
if.R(s={
  ## multicomp must be done with apple.ancova.2

  ## S+ 8.1.1 has a bug in multcomp.default
  ## This example requires a patch or an update to S+
  
  apple.mmc <-
    multicomp.mmc(apple.ancova.2, focus="treat",
                  comparisons="mcc", method="dunnett", valid.check=FALSE,
                  plot=FALSE)
  apple.mmc

  old.mar <- par(mar=c(15,4,4,2)+.1)

  plot(apple.mmc, col.iso=16, x.offset=10)

  par(mar=c(-4,4,28,2)+.1, new=TRUE)
  plotMatchMMC(apple.mmc$mca)

  par(old.mar)
  
},r={
  ## glht must be done with apple.ancova.2
  
  apple.mmc <- mmc(apple.ancova.2,
                    ## linfct=mcp(treat="Dunnett", base=6), ## not yet
                    linfct = mcp(treat=contrMat(rep(4,6), base=6)))
  apple.mmc

  old.omd <- par(omd=c(0,1,.3,1))
  plot(apple.mmc, col.iso=16, x.offset=15, col.mca.signif="red")
  par(omd=c(0,1,0,.4), new=TRUE)
  plotMatchMMC(apple.mmc$mca)
  par(old.omd)

})
