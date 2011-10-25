source("c:/HOME/rmh/HH-R.package/HH/R/interaction2wt-new.R")

i2wt <- interaction2wt(Yield ~ Soil + Trt, data=barleyp,
                       par.strip.text=list(cex=1.4),
                       main.cex=1.6,
                       scales=list(
                         x=list(cex=.5, rot=90),  ## the cex and rot for the x scale is ignored.
                         y=list(cex=.9, alternating=3)),
                       rot=c(90,0)  ## the rot is honored
                       )
i2wt
i2wt <- interaction2wt(Yield ~ Soil + Trt, data=barleyp,
                       par.strip.text=list(cex=1.4),
                       main.cex=1.6,
                       scales=list(
                         x=list(cex=.5, rot=90),  ## the cex and rot for the x scale is honored, but not updateable
                         y=list(cex=1, alternating=3)))
i2wt
