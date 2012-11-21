## This file is a superset of the examples in file ae.dotplot7.Rd

## They are here because they are excessive for the main documentation
## and I didn't want to lose them.


## variable names in the input data.frame aeanonym
## RAND   treatment as randomized
## PREF   adverse event symptom name
## SN     number of patients in treatment group
## SAE    number of patients  in each group for whom the event PREF was observed
## OrgSys Organ System
##
## Input sort order is PREF/RAND

data(aeanonym)
head(aeanonym)

## variable names are hard-wired in the program
## names(aeanonym) <- c("RAND", "PREF", "SAE", "SN", "OrgSys")


## Calculate log relative risk and confidence intervals (95% by default).
## AElogrelrisk sets the sort order for PREF to match the relative risk.
aeanonymr <- AElogrelrisk(aeanonym) ## levels of PREF are sorted by relative risk
head(aeanonymr)
class(aeanonymr$PREF)
levels(aeanonymr$PREF)


AEdotplot(aeanonym)
AEdotplot(aeanonym, sort=FALSE)
aeanonymrev <- aeanonym
aeanonymrev$PREF <- ordered(aeanonymrev$PREF, rev(unique(aeanonymrev$PREF)))
AEdotplot(aeanonymrev, sort=FALSE) ## the original dataset was ordered by B.pct
AEdotplot(aeanonym, conditionName="ALL")
AEdotplot(aeanonym, conditionName="ALL", sub=list("ABC-1234", cex=.7))
AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys)
AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys, useCondition=FALSE)
AEdotplot(aeanonym, conditionVariable=rep(c("UVW","XYZ"), c(44,22)))

AE6 <- AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys)
AE6
names(attributes(AE6))
names(attributes(AE6)$AEtable)
head(attributes(AE6)$AEtable$Misc)
AEdotplot(attributes(AE6)$AEtable$Misc)
AEdotplot(attributes(AE6)$AEtable$Misc, conditionName="Misc")

KEEP <- aeanonym$OrgSys %in% c("GI","Resp")
ae1.dp <- AEdotplot(aeanonym[KEEP,], conditionVariable=aeanonym$OrgSys[KEEP])
ae1.dp
attr(ae1.dp, "sub") <- list(paste("GI", "Resp"), cex=.7)
ae1.dp
ae2.dp <- AEdotplot(aeanonym[aeanonym$OrgSys %in% "Misc",], conditionName="Misc")
ae2.dp
ae12.dp <- c(ae1.dp, ae2.dp)
ae12.dp ## incomplete subtitle
attr(ae12.dp, "sub") <- list(paste("GI", "Resp", "Misc"), cex=.7)
ae12.dp

names(attributes(ae1.dp))
names(attributes(ae2.dp))
names(attributes(ae12.dp))
names(attributes(ae1.dp)$AEtable)
names(attributes(ae2.dp)$AEtable)
names(attributes(ae12.dp)$AEtable)
lapply(attributes(ae12.dp)$AEtable, head)

print(ae12.dp, AEtable=FALSE) ## display only the first two panels.
print(ae12.dp, panel.widths=c(.70, .30, 0))   ## same as above
print(ae12.dp, panel.widths=c(.60, .20, .2))  ## control over widths
print(ae12.dp, panel.widths=c(.65, .15, .20))



aefake <- rbind(cbind(aeanonym, group="ABC"), cbind(aeanonym, group="DEF"))
aefake$SAE[67:132] <- sample(aefake$SAE[67:132])

## fake 0 ## ABC and DEF have different sort orders for PREF
AEfake <- AEdotplot(aefake, conditionVariable=aefake$group,
          sub=list("ABC and DEF have different sort orders for PREF", cex=.7))
AEfake ## ABC and DEF have different sort orders for PREF

aefake$OrgSys.group <- with(aefake, interaction(OrgSys, group))
AEfake <- AEdotplot(aefake, conditionVariable=aefake$OrgSys.group,
          sub=list("ABC and DEF have different sort orders for PREF", cex=.7))
AEfake ## ABC and DEF have different sort orders for PREF


## fake 1
AE1 <- AEdotplot(aefake[ 1: 66,], conditionVariable=aefake$OrgSys[ 1: 66])
AE2 <- AEdotplot(aefake[67:132,], conditionVariable=aefake$OrgSys[67:132])
AE1
AE2
## AE1 and AE2 have different sort orders for PREF
## to change the AE2 sort order to match AE1 use
AEmatchSortorder(AE1, AE2)

## fake 2
KEEP <- aefake$OrgSys %in% c("GI","Resp")
AEfakeGR <- AEdotplot(aefake[KEEP,], conditionVariable=aefake$OrgSys.group[KEEP],
            sub=list("ABC and DEF have different sort orders for PREF", cex=.7))
AEfakeGR ## ABC and DEF have different sort orders for PREF
AEfakeGR1 <- AEdotplot(aefake[KEEP & (1:132) <= 66,],
                       conditionVariable=aefake$OrgSys.group[KEEP & (1:132) <= 66])
AEfakeGR2 <- AEdotplot(aefake[KEEP & (1:132) >= 67,],
                       conditionVariable=aefake$OrgSys.group[KEEP & (1:132) >= 67])
AEfakeGR1
AEfakeGR2
AEmatchSortorder(AEfakeGR1, AEfakeGR2)

## suppress subtitle
attr(ae1.dp, "sub") <- list("")
ae1.dp
## change cex for the various sets of labels
ae1.dp <- update(ae1.dp,
                 scales=list(x=list(cex=1.2), y=list(cex=.4)),
                 par.strip.text=list(cex=.9))

## Just a few formula method examples here.
## See ?AEdotplot for more formula examples
AEdotplot(PREF ~ SAE/SN | OrgSys, groups = RAND, data = aeanonym)

## AEdata is the same dataset as aeanonym but with different variable names
data(AEdata)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)

## test sortbyRelativeRisk=FALSE
ABCD.12345 <- AEdata[1:12,]
head(ABCD.12345)
AEdotplot(AE ~ nAE/nTRT, groups=TRT, data=ABCD.12345)
AEdotplot(AE ~ nAE/nTRT, groups=TRT, data=ABCD.12345, sort=FALSE)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups=TRT, data=ABCD.12345)
AEdotplot(AE ~ nAE/nTRT | OrgSys, groups=TRT, data=ABCD.12345, sort=FALSE)
AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys)
AEdotplot(aeanonym, conditionVariable=aeanonym$OrgSys, sort=FALSE)
AEmatchSortorder(AE1, AE2)
AEmatchSortorder(AEfakeGR1, AEfakeGR2)

## zero values for nAE (SAE) are handled correctly
aeanonym1 <- aeanonym[1:12,]
aeanonym1[c(1,2,3,6),3] <- 0
AEdotplot(aeanonym1)

if (FALSE) {
## these phrases don't work
try(AEdotplot(aeanonym[,1:4], conditionName=aeanonym[,5]))    ## conditionName must be a
try(AEdotplot(aeanonym[,1:4], conditionName=aeanonym$OrgSys)) ## character scalar
try(AEdotplot(aeanonym[,1:4], conditionName=123))        ##
try(AEdotplot(aeanonym, subset=(aeanonym$OrgSys %in% c("GI","Resp")))) ## subset= doesn't work
try(AEdotplot(aefake, conditionVariable=aefake$OrgSys)) ## At least one Adverse Effect has other than two Treatment levels.
try(AEdotplot(PREF ~ SAE/SN | OrgSys, data = AEdata)) ## not valid, missing Treatment variable

old.value <- aeanonym$SN[1]
aeanonym$SN[1] <- 0
try(AEdotplot(aeanonym)) ## not valid, At least one AE has 0 patients at risk.
aeanonym$SN[1] <- old.value
}
