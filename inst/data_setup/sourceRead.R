setwd("c:/HOME/rmh/HH-R.package/HH/src/")
require(vcd)
require(HH)
source("readHHdata.R", echo=TRUE)


## save(list=ls(), file="../data/HH.rda")  ## suppress .Random.seed
## load("../data/HH.rda")


rm(i)
for (i in ls()) save(list=i, file=paste("c:/HOME/rmh/HH-R.package/HH/data/", i, ".rda", sep=""))
rm(i)
