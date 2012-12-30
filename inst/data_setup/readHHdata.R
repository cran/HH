## R:
##  require(HH)
##  old.HH.ROOT.DIR <- options(HH.ROOT.DIR="c:/HOME/rmh/HH-R.package/HH/inst")
##  source("c:/HOME/rmh/HH-R.package/HH/inst/data_setup/readHHdata.R", echo=TRUE, print.eval=TRUE)
##  options(old.HH.ROOT.DIR)

## S-Plus:
##  getwd()
##  require(HH)
##  old.HH.ROOT.DIR <- options(HH.ROOT.DIR="c:/HOME/rmh/HH-R.package/HH/inst")
##  source("c:/HOME/rmh/HH-R.package/HH/inst/data_setup/readHHdata.R", echo=TRUE, auto.print=TRUE)
##  options(old.HH.ROOT.DIR)
##  rm(old.HH.ROOT.DIR)
##
##  rm(.First.lib, .Last.value, .Random.seed)
##  q()
## The file paste(getwd(), ".Data", sep="/")
## contains the data objects.

## Manually rename the .Data directory to
## "c:/HOME/rmh/HH-R.package/HH/inst/HH.Data"
## Manually remove the file c:/HOME/rmh/HH-R.package/HH/inst/HH.Data/.Audit

if.R(r={},
     s={
       as.table <- function(x) {
         class(x) <- c("table", oldClass(x))
         x
       }})


R282 <- read.table(hh("datasets/2.8-2-full.dat"), header=TRUE)
R282$blocks <- factor(R282$blocks)
for (i in 3:10) R282[[i]] <- factor(R282[[i]])
head(R282)

`Design_2.8_2`            <- read.table(hh("datasets/2.8-2.dat"), header=FALSE, col.names=c("blocks","trt"))                   ; head(`Design_2.8_2`            )
abrasion                <- read.table(hh("datasets/abrasion.dat"), header=TRUE)                 ; head(abrasion                )

acacia <- matrix(c(2,13,10,3), nrow=2, byrow=TRUE,
                 dimnames=list(
                   Species=c("A","B"),
                   invasion=c("Not.Inv","Invaded")))
acacia <- as.table(acacia)

aeanonym <- read.table(hh("datasets/aedotplot.dat"), header=TRUE, sep=",") ; head(aedotplot)
animal                  <- read.table(hh("datasets/animal.dat"), header=TRUE)                   ; head(animal                  )
anneal                  <- read.table(hh("datasets/anneal.dat"), header=TRUE)                   ; head(anneal                  )

apple <- read.table(hh("datasets/apple.dat"), header=TRUE)
apple$treat <- factor(apple$treat)
contrasts(apple$treat) <- contr.treatment(6)
apple$block <- factor(apple$block)
head(apple)

ara                     <- read.table(hh("datasets/ara.dat"), header=TRUE)                      ; head(ara                     )
balance                 <- read.table(hh("datasets/balance.dat"), header=FALSE, col.names=c("sway","age"))                  ; head(balance                 )
barleyp                 <- read.table(hh("datasets/barleyp.dat"), header=TRUE)                  ; head(barleyp                 )
batch                   <- read.table(hh("datasets/batch.dat"), header=TRUE)   ; batch$Batch <- factor(batch$Batch)                 ; head(batch                   )
bean                    <- read.table(hh("datasets/bean.dat"), header=TRUE)                     ; head(bean                    )
birthweight             <- read.table(hh("datasets/birthweight.dat"), header=TRUE);  birthweight$gender <- factor(birthweight$gender, labels=c("F","M"))            ; head(birthweight             )
blood                   <- read.table(hh("datasets/blood.dat"), header=FALSE, col.names=c("times","diets")) ; blood$diets <- factor(blood$diets, labels=LETTERS[1:4])                   ; head(blood                   )

## blyth <- read.table(hh("datasets/blyth.dat"), header=TRUE)
## head(blyth )
blyth.df <- read.table(hh("datasets/blyth.dat"), header=TRUE)
blyth.df$treatment <- ordered(blyth.df$treatment, levels=c("standard","new"))
tmp <- tapply(blyth.df$count, blyth.df[,-1], c)
class(tmp) <- c("table","array")
tmp
##
blyth <- matrix(tmp, 2, 4)
dimnames(blyth) <- list(dimnames(tmp)[[1]],
                            interaction(blyth.df[c(1,3,5,7),4:3]))
rm(tmp, blyth.df)
blyth

breast                  <- read.table(hh("datasets/breast.dat"), header=TRUE)                   ; head(breast                  )

budworm <- read.table(hh("datasets/budworm.dat"), header=TRUE)
names(budworm)[[2]] <- "numdead"
head(budworm )

byss <- read.table(hh("datasets/byss.dat"),
                   col.names=c("yes","no","dust","race","sex",
                               "smoker","emp.length"))
byss$dust <- factor(byss$dust, levels=c(3,2,1),
                    labels=c("low", "medium","high"))
byss$race <- factor(byss$race, levels=c(1,2), labels=c("white","other"))
byss$sex <- factor(byss$sex, levels=c(1,2), labels=c("male","female"))
byss$smoker <- factor(byss$smoker, levels=c(2,1), labels=c("no","yes"))
byss$emp.length <- factor(byss$emp.length, levels=c(1,2,3),
                      labels=c("< 10 years","10-20 years","> 20 years"))
head(byss                    )

c3c4                    <- read.table(hh("datasets/c3c4.dat"), header=TRUE)                     ; head(c3c4                    )

catalystm <- read.table(hh("datasets/catalystm.dat"), header=FALSE,
                       col.names=c("catalyst","concent"))
catalystm$catalyst <- factor(catalystm$catalyst, labels=c("A","B","C","D"))
head(catalystm               )

cc135 <- read.table(hh("datasets/cc135.dat"), header=TRUE)
names(cc135)[8] <- "res.treat"
cc135$treat   <- factor(cc135$treat,
      labels=c("roughage","limited.grain","full.grain"))
cc135$period  <- factor(cc135$period)
cc135$cow     <- factor(cc135$cow)
cc135$square  <- factor(cc135$square)
cc135$res.treat <- factor(cc135$res.treat, levels=c(1:3,0),
      labels=c("roughage","limited.grain","full.grain","none"))
cc135$sequence <- factor(cc135$sequence)
cc135$nores    <- factor(cc135$nores, labels=c("None","Res"))
head(cc135)

cc176 <- matrix(scan(hh("datasets/cc176.dat")), ncol=2, byrow=TRUE)
current.levels <- c("galvanic","faradic","60.cycle","25.cycle")
cc176 <-
  data.frame(wt.d=cc176[,1],
             wt.n=cc176[,2],
             n.treats=ordered(rep(c(1,3,6), length=96)),
             current=ordered(rep(rep(current.levels,
               rep(3,4)),8), levels=current.levels),
             minutes=ordered(rep(rep(c(1,2,3,5),rep(12,4)), 2)),
             rep=rep(c("I","II"), c(48,48)))
       dimnames(contrasts(cc176$current))[[1]] <- levels(cc176$current) ## needed in R
       dimnames(contrasts(cc176$minutes))[[1]] <- levels(cc176$minutes) ## needed in R
rm(current.levels)
## contrasts(cc176$n.treats)
contrasts(cc176$n.treats) <-
  if.R(s=
       contr.poly(c(1,3,6))
       ,r=
       contr.poly(3, scores=c(1,3,6))
       )
## contrasts(cc176$n.treats)
head(cc176                   )

cement                  <- read.table(hh("datasets/cement.dat"), header=FALSE, col.names=c("heat","conc1","conc2","conc3","conc4"))                   ; head(cement                  ) ## datasets-maybe/NIST.dat/regb/cement.txt
census4                 <- read.table(hh("datasets/census4.dat"), header=FALSE)                  ; head(census4                 ) ## ?
cereals                 <- read.table(hh("datasets/cereals.dat"), header=TRUE)                  ; head(cereals                 )
## cereals.info            <- read.table(hh("datasets/cereals.info"), header=TRUE)                 ; head(cereals.info            )
chimp                   <- read.table(hh("datasets/chimp.dat"), header=TRUE)                    ; head(chimp                   )

circuit <- read.table(hh("datasets/circuit.dat"), header=TRUE)
for (i in 1:5) circuit[,i] <- factor(circuit[,i])
head(circuit)

co2 <- if.R(r=datasets::co2,      ## Use the builtin R data set
            s={
              co2 <- as.rts(get("co2", "data")) ## rts of the builtin S-Plus data set
              attr(attr(co2, "tspar"), "units") <- "months"
            }
            )
head(co2)

concord                 <- read.table(hh("datasets/concord.dat"), header=TRUE)                  ; head(concord                 )

crash <- read.table(hh("datasets/crash.dat"), header=TRUE)
crash$passengers <- ordered(crash$passengers)
position(crash$agerange) <- c(1.2, 2.5, 3.7)
head(crash )

## crash.datatable         <- read.table(hh("datasets/crash.datatable"), header=TRUE)              ; head(crash.datatable         )

crime                   <- as.table(data.matrix(read.table(hh("datasets/crime.dat"), header=TRUE)))                    ; head(crime                   )
darwin                  <- read.table(hh("datasets/darwin.dat"), header=FALSE, col.names=c("cross","self"))                   ; head(darwin                  )
diamond                 <- read.table(hh("datasets/diamond.dat"), header=TRUE)                  ; head(diamond                 )

display <- read.table(hh("datasets/display.dat"), header=TRUE)
display$panel <- factor(display$panel)
display$panel.ordered <- factor(display$panel)
position(display$panel.ordered) <- (1:3)+.5
display$emergenc <- factor(display$emergenc)
head(display)

distress                <- read.table(hh("datasets/distress.dat"), header=FALSE, col.names=c("birthwt","outcome"))                 ; head(distress                )

## draft70mn               <- read.table(hh("datasets/draft70mn.dat"), header=TRUE)                ; head(draft70mn               )
if.R(r={
  draft70mn <- read.fwf(hh("datasets/draft70mn.dat"), header=FALSE,
                        widths=c(5,rep(4,11)),
                        col.names=month.abb)
  draft <- stack(draft70mn)
  names(draft) <- c("rank","month")
}, s={
  draft70mn <- read.table(hh("datasets/draft70mn.dat"), header=FALSE,
                          sep=seq(3,47,4),
                          col.names=month.abb)
  draft <- data.frame(rank=unlist(draft70mn),
                      month=rep(names(draft70mn), nrow(draft70mn)))
})
head(draft70mn)
draft$monthOrd <- ordered(draft$month, month.abb)
head(draft)
levels(draft$month)
levels(draft$monthOrd)

drunk <- if.R(r={
  read.table(hh("datasets/drunk.dat"),
                    col.names=c('0-29','30-39','40-49','50-59','>=60'),
                    row.names=c("males","females"),
                    check.names=FALSE)
}, s={
  read.table(hh("datasets/drunk.dat"),
                    col.names=c('0-29','30-39','40-49','50-59','>=60'),
                    row.names=c("males","females"))
})
drunk <- as.table(data.matrix(drunk))
names(dimnames(drunk)) <- c("sex","age")
head(drunk)

eggs                    <- read.table(hh("datasets/eggs.dat"), header=TRUE)                     ; head(eggs                    )

elnino <- ts(scan(hh("datasets/elnino.dat")), start=c(1955,1), frequency=12)
ts(elnino[1:24], start=c(1955,1), frequency=12)
## http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4412.htm
## c:/HOME/rmh/504.s05/0407/elnino.txt
## c:/HOME/rmh/504.s05/0407/elnino.text

employM16 <- read.table(hh("datasets/employM16.dat"))[,5:16]
employM16 <- if.R(r=ts(as.vector(t(employM16)), start=c(1948,1), frequency=12),
                  s=rts(as.vector(t(data.matrix(m16))),
                    start=1948, frequency=12, units="months"))

energy <- read.table(hh("datasets/energy.dat"), header=TRUE, sep=",")
  ## energy <- if.R(r=read.table(hh("datasets/energy.dat"), header=TRUE, sep=","),
  ##                s=importData(hh("datasets/energy.dat")))
energy$Wood <- ordered(energy$Wood,
                       levels=c(
                         "Osage Orange",
                         "Red Oak",
                         "Black Walnut",
                          "White Pine"))
energy$Stove <- factor(energy$Stove, labels=c("A","B","C"))
## energy <- energy[order(energy$Moist, energy$Wood, energy$Kind),]
head(energy)

esr <- read.table(hh("datasets/esr.dat"), header=FALSE, col.names=c("fibrin","gammaglob","ESR")) ; head(esr)

fabricwear              <- read.table(hh("datasets/fabricwear.dat"), header=FALSE, col.names=c("speed","wear"))
fabricwear$speed <- ordered(fabricwear$speed)
head(fabricwear)

fat.dat <- read.table(hh("datasets/fat.dat"), header=FALSE, ## hh/datasets/fat.htm
                      col.names=
                      c("Case","bodyfat.Brozek","bodyfat.Siri","Density","Age",
                        "Weight","Height","Adiposity","FatFreeWeight","Neck","Chest","Abdomen",
                        "Hip","Thigh","Knee","Ankle","ExtendedBiceps","Forearm","Wrist"))
head(fat.dat)

fat                     <- read.table(hh("datasets/fat.data"), header=TRUE)                     ; head(fat                     )
## fat.htm                 <- read.table(hh("datasets/fat.htm"), header=TRUE)                      ; head(fat.htm                 )
## fat.txt                 <- read.table(hh("datasets/fat.txt"), header=TRUE)                      ; head(fat.txt                 )

feed <- read.table(hh("datasets/feed.dat"), header=TRUE)
feed$temp <- ordered(feed$temp)
feed$supp <- ordered(feed$supp)
position(feed$temp) <- 2:4
position(feed$supp) <- 1:5
head(feed)

filmcoat <- read.table(hh("datasets/filmcoat.dat"),
                       col.names=c("temprt","pressure","coat"))
filmcoat$temprt <- factor(filmcoat$temprt,
                          labels=c("t.low","t.med","t.high"))
filmcoat$pressure <- factor(filmcoat$pressure,
                            labels=c("p.low","p.med","p.high"))
head(filmcoat                )

filter                  <- read.table(hh("datasets/filter.dat"), header=TRUE)                   ; head(filter                  )
fruitflies              <- read.table(hh("datasets/fruitflies.dat"), header=TRUE)               ; head(fruitflies              )
furnace                 <- read.table(hh("datasets/furnace.dat"), header=TRUE)                  ; head(furnace                 )
girlht                  <- read.table(hh("datasets/girlht.dat"), header=TRUE)                   ; head(girlht                  )

glasses                 <- read.table(hh("datasets/glasses.dat"), header=FALSE)
glasses <- as.table(matrix(unlist(glasses),
                           nrow=2,
                           dimnames=list(
                             wearer=c("glasses","no.glasses"),
                             delinquent=c("delinq","non.del"))))
glasses

golf                    <- read.table(hh("datasets/golf.dat"), header=TRUE, na=".")                     ; head(golf                    )

## Glantz and Slinker, page 437
gum.in <- read.table(hh("datasets/gum.dat"), header=TRUE)
gum <- data.frame(Gum=rep(rep(c("Standard","Bicarbonate"), c(8,8)), 3))
## the authors report time as (20,30,50) in minutes from eating candy.
## we use Time in 10 minute units, counting from 20 minutes after eating candy,
## then chew gum for 10 minutes, then wait another 20 minutes.
gum$Time <- ordered(rep(c(20,30,50), c(16,16,16)))
contrasts(gum$Time) <- if.R(r=contr.poly(3, c(20,30,50)),
                            s=contr.poly(c(20,30,50)))
gum$subject <- factor(rep(1:8, 6))
gum$pH <-  unlist(gum.in[2:4])
rm(gum.in)
head(gum)

gunload <- read.table(hh("datasets/gunload.dat"), header=TRUE)
gunload$method <- factor(gunload$method, labels=c("new","old"))
gunload$group <- factor(gunload$group, labels=c("slight","average","heavy"))
gunload$team <- factor(gunload$team)
head(gunload)

har1                    <- read.table(hh("datasets/har1.dat"), header=TRUE)                     ; head(har1                    )

if.R(r={
  har2 <- read.table(hh("datasets/har2.dat"), header=TRUE, fill=TRUE)
  har2 <- stack(har2)
  names(har2) <- c("weight", "treatment")
  levels(har2$treatment) <- c("control","treated")
}, s={
  har2 <- importData(hh("datasets/har2.dat"), type="ASCII", colNameRow=1)
  har2 <- data.frame(weight=unlist(har2),
                     treatment=rep(c("treated", "control"), each=nrow(har2)))
  row.names(har2) <- 1:nrow(har2)
})
har2 <- har2[!is.na(har2$weight),]
head(har2)

har3 <- read.table(hh("datasets/har3.dat"), header=FALSE, col.names=c("degrees","age"))
har3$age <- factor(har3$age, labels=c("36-","37+"))
head(har3)

hardness                <- read.table(hh("datasets/hardness.dat"), header=TRUE)                 ; head(hardness                )
heartvalve              <- read.table(hh("datasets/heartvalve.dat"), header=TRUE)               ; head(heartvalve              )

hooppine <- read.table(hh("datasets/hooppine.dat"), header=FALSE,
                       col.names=c("temp","tree","strength","moisture"))
hooppine$temp <- ordered(hooppine$temp)
row.names(contrasts(hooppine$temp)) <- levels(hooppine$temp)
hooppine$tree <- factor(hooppine$tree)
head(hooppine)

hospital                <- read.table(hh("datasets/hospital.dat"), header=TRUE)                 ; head(hospital                )
hotdog                  <- read.table(hh("datasets/hotdog.dat"), header=TRUE)                   ; head(hotdog                  )

hpErie <- read.table(hh("datasets/houseprice-erie.dat"), header=FALSE)
names(hpErie) <- c("price","taxes","bathrm","lotsize","sqfeet",
                    "garage","rooms","bedrm","age","type","style","fireplac")
hpErie$type <- factor(hpErie$type,
                       labels=c("brick","brick&frame","alum&frame","frame"))
hpErie$style <- factor(hpErie$style,
                       labels=c("2.story","1.5.story","ranch"))
head(hpErie)

houseprice              <- read.table(hh("datasets/houseprice.dat"), header=TRUE)               ; head(houseprice              )
htwt                    <- read.table(hh("datasets/htwt.dat"), header=TRUE, na="*");   htwt$ht <- 12*htwt$feet + htwt$inch                  ; head(htwt                    )

## iceskate                <- read.table(hh("datasets/iceskate"), header=TRUE)                     ; head(iceskate                )
## iceskate_w3_saved.txt   <- read.table(hh("datasets/iceskate-w3-saved.txt"), header=TRUE)        ; head(iceskate_w3_saved.txt   )
## iceskate_w3.awk         <- read.table(hh("datasets/iceskate-w3.awk"), header=TRUE)              ; head(iceskate_w3.awk         )
## iceskate_w3             <- read.table(hh("datasets/iceskate-w3.dat"), header=TRUE)              ; head(iceskate_w3             )
## iceskate_w3.sh          <- read.table(hh("datasets/iceskate-w3.sh"), header=TRUE)               ; head(iceskate_w3.sh          )
## iceskate_w3.txt         <- read.table(hh("datasets/iceskate-w3.txt"), header=TRUE)              ; head(iceskate_w3.txt         )
## iceskate.cmt            <- read.table(hh("datasets/iceskate.cmt"), header=TRUE)                 ; head(iceskate.cmt            )
## iceskate.htm            <- read.table(hh("datasets/iceskate.htm"), header=TRUE)                 ; head(iceskate.htm            )

## 2002  OLYMPICS WOMEN'S SKATING FINALS
iceskate <- scan(hh("datasets/iceskate-w3.dat"), what="", multi.line=TRUE)
iceskate <- matrix(iceskate, 23, 28, byrow=TRUE)
iceskate <- data.frame(country=iceskate[,2],
                       technical=iceskate[,c(7:15)],
                       presentation=iceskate[,c(20:28)],
                       row.names=paste(iceskate[,3], iceskate[,4]))
for (i in 2:19) iceskate[,i] <- as.numeric(as.character(iceskate[,i]))
if.R(r=rm(i), s={})
head(iceskate)


icu <- read.table(hh("datasets/icu.dat"), header=FALSE,
                  row.names="ID",
                  col.names=c("ID",
                    "STA","AGE","SEX","RACE","SER","CAN",
                    "CRN","INF","CPR","SYS","HRA","PRE","TYP",
                    "FRA","PO2","PH","PCO","BIC","CRE","LOC"))
icu$jSTA <- jitter(icu$STA, if.R(s=3, r=.25))
icu$STA <- factor(icu$STA, levels=c(0,1), labels=c("Lived","Died"))
icu$SEX <- factor(icu$SEX, levels=c(0,1), labels=c("Male","Female"))
icu$CPR <- factor(icu$CPR, levels=c(0,1), labels=c("No","Yes"))
head(icu)

income                  <- read.table(hh("datasets/income.dat"), header=TRUE)                   ; head(income                  )
inconsistent            <- read.table(hh("datasets/inconsistent.dat"), header=TRUE)             ; head(inconsistent            )

## intubate                <- read.table(hh("datasets/intubate.dat"), header=TRUE)                 ; head(intubate                )
intubate <- read.table(hh("datasets/intubate.dat"), header=FALSE,
                       col.names=c("center", "vent", "invas", "Freq"))
intubate$vent <- factor(intubate$vent,
                        levels=c("yes","no"),
                        labels=c("vent.ther","not.vent"))
intubate$invas <- factor(intubate$invas,
                             levels=c("yes","no"),
                             labels=c("invas.intub","not.invas"))
if.R(r={
  require(vcd)
  structable( ~ center + invas + vent, direction=c("v","v","h"), data=intubate)
}, s={
  tmp <- tapply(intubate$Freq, intubate[, c(2,3,1)], c)
  names(dimnames(tmp)) <- c("vent","invas","center")
  print(as.table(tmp))
  rm(tmp)
})

ironpot                 <- read.table(hh("datasets/ironpot.dat"), header=TRUE)                  ; head(ironpot                 )
kangaroo                <- read.table(hh("datasets/kangaroo.dat"), header=TRUE, skip=1)                 ; head(kangaroo                )
kidney                  <- read.table(hh("datasets/kidney.dat"), header=TRUE)                   ; head(kidney                  )
kyphosis                <- read.table(hh("datasets/kyphosis.dat"), header=FALSE, col.names=c("row.names","Kyphosis","Age","Number","Start"), row.names=1)                 ; head(kyphosis                )
lake                    <- read.table(hh("datasets/lake.dat"), header=TRUE)                     ; head(lake                    )
## leukemia.lee            <- read.table(hh("datasets/leukemia.lee.dat"), header=TRUE)             ; head(leukemia.lee            )
## leukemia.lee$response <- factor(leukemia.lee$response)

leukemia <- read.table(hh("datasets/leukemia.dat"), header=FALSE,
                       col.names=c("age","smear","infiltrate","labeling",
                         "blasts","temp","response","time","status"))
head(leukemia)

lft.asat                <- read.table(hh("datasets/lft.asat.dat"), header=TRUE,sep=",")                 ; head(lft.asat                )
lifeins                 <- read.table(hh("datasets/lifeins.dat"), header=TRUE)                  ; head(lifeins                 )

## longley <- read.table(hh("datasets/longley.dat"), header=FALSE)
## head(longley                 )
longley <- if.R(r=datasets::longley,
                s=data.frame(longley.x, Employed = longley.y))
head(longley)

lymph <- read.table(hh("datasets/lymph.dat"), header=TRUE)
names(lymph)[1] <- "nodes"
lymph$X.ray <- factor(lymph$X.ray)
lymph$stage <- factor(lymph$stage)
lymph$grade <- factor(lymph$grade)
## We will be jittering the 0/1 values of nodes to increase the
## visibility of the individual points.  Since we will do this
## in many graphs, we jitter them once and reuse the jittered
## points in all plots.
lymph$nodes.j <- jitter(lymph$nodes,
                        if.R(s=5, r=.5))
lymph$nodes <- factor(lymph$nodes)
head(lymph)

maiz <- read.table(hh("datasets/maiz.dat"), header=TRUE)
maiz$hibrido <- factor(maiz$hibrido,
                       levels=c("P3747","P3732","Mol17","A632","LH74"))
maiz$nitrogeno <- factor(maiz$nitrogeno)
position(maiz$nitrogeno) <- c(1.2, 2.4, 3.6, 4.8)
position(maiz$bloque) <- c(2.25, 3.75)


manhours                <- read.table(hh("datasets/manhours.dat"), header=FALSE,
                                      col.names=c("manhours", "occupanc", "checkins", "svcdesk",
                                        "common", "wings", "berthing", "rooms"))                ; head(manhours                )
market                  <- read.table(hh("datasets/market.dat"), header=TRUE)                   ; head(market                  )
mice                    <- read.table(hh("datasets/mice.dat"), header=TRUE)                     ; head(mice                    )
mileage                 <- read.table(hh("datasets/mileage.dat"), header=TRUE)                  ; head(mileage                 )

mortality <- if.R(r={
  read.fwf(hh("datasets/mortality.dat"), header=FALSE,
           col.names=c("x","birthweight","dead","alive"),
           row.names="birthweight", skip=3, widths=c(1,10,9,14))[,-1]
}, s={
  read.table(hh("datasets/mortality.dat"), header=FALSE,
             col.names=c("birthweight","dead","alive"),
             row.names="birthweight", skip=3, sep=c(2,12,21))
})
mortality <- as.table(data.matrix(mortality))
names(dimnames(mortality)) <- c("birthweight","outcome")
mortality

mpg                     <- read.table(hh("datasets/mpg.dat"), header=TRUE)                      ; head(mpg                     )
muscle                  <- read.table(hh("datasets/muscle.dat"), header=TRUE)                   ; head(muscle                  )
source(hh("data_setup/njgolf-read.r")) ; head(njgolf)
normtemp                <- read.table(hh("datasets/normtemp.dat"), header=FALSE, col.names=c("BodyTempF","Gender","HeartRate"))
normtemp$Gender=factor(normtemp$Gender, labels=c("male","female"))
head(normtemp) ## Journal of Statistics Education (Shoemaker 1996)

notch <- read.table(hh("datasets/notch.dat"), header=FALSE,
                    col.names=c("energy","machine"))
notch$machine <- factor(notch$machine,
                        labels=c("Tinius1","Tinius2","Satec","Tokyo"))
head(notch)

## nottem <- read.table(hh("datasets/nottem.dat"), header=TRUE)
## head(nottem)

if.R(r={nottem <- datasets::nottem},
     s={
       library(MASS)
       nottem <- get("nottem", where="MASS")
       detach("MASS")
     }
     )
head(nottem)

oats                    <- read.table(hh("datasets/oats.dat"), header=TRUE)                     ; head(oats                    )
odoffna                 <- read.table(hh("datasets/odoffna.dat"), header=TRUE)                  ; head(odoffna                 )
operator                <- read.table(hh("datasets/operator.dat"), header=TRUE)                 ; head(operator                )

ozone <- read.table(hh("datasets/ozone.dat"), na.strings="-9999")
ozone <- if.R(r=ts(as.vector(t(ozone[,5:16])), start=c(1926,1), freq=12),
              s=rts(as.vector(t(arosa[,5:16])), start=c(1926,1), freq=12))
ts(ozone[1:36], start=c(1926,1), frequency=12)

paper                   <- read.table(hh("datasets/paper.dat"), header=FALSE, col.names=paste("material", 1:5, sep="."))
## From Hoaglin, Mosteller, Tukey,
## Fundamentals of Exploratory Analysis of Variance
## Wiley 1991
## page 375
paper <- data.frame(smoothness=unlist(paper),
                    material=factor(rep(1:5, rep(32,5))))
row.names(paper) <- if.R(r=NULL,
                         s=1:nrow(paper))
head(paper)

## patient <- read.table(hh("datasets/patient.dat"), header=TRUE)
## head(patient)
if.R(r={
  tmp <- read.fwf(hh("datasets/patient.dat"),
                  width=c(8,8,8,8,8),
                  col.names=c("stomach","bronchus",
                    "colon","ovary","breast"))
  patient <- stack(tmp)
  names(patient) <- c("surv.time", "organ")
}, s={
  tmp <- read.table(hh("datasets/patient.dat"),
                    sep=c(1,9,17,25,32),
                    col.names=c("stomach","bronchus",
                      "colon","ovary","breast"))
  patient <- data.frame(surv.time=unlist(tmp), organ=rep(names(tmp), each=nrow(tmp)))
})
patient <- patient[!is.na(patient$surv.time),]
head(patient)
rm(tmp)

plasma <- data.frame(plasma=scan(hh("datasets/plasma.dat")),
                     time=factor(rep(1:5, 10),
                       labels=c("8am","11am","2pm","5pm","8pm")),
                     id=factor(rep(1:10, rep(5,10))))
head(plasma)

political               <- read.table(hh("datasets/political.dat"), header=TRUE)
political <- as.table(data.matrix(political))
names(dimnames(political)) <- c("leaning","parentalDecisionMaking")
political

potency <- read.table(hh("datasets/potency.dat"), header=FALSE, col.names=c("potency","temp"))
potency$temp <- ordered(potency$temp)
head(potency)

pox                     <- read.table(hh("datasets/pox.dat"), header=FALSE, col.names=c("admission","later"))                      ; head(pox                     )

product <- scan(hh("datasets/product.dat"))
product <- if.R(r=ts(diff(product)),
                s=rts(diff(product)))
ts(product[1:20])

psycho <- read.table(hh("datasets/psycho.dat"), header=FALSE,
                     col.names=c("sex","age.group","mean.age",
                                 "GHQ","n.taking","n.total"))
psycho$sex <- factor(psycho$sex, levels=c(0,1), labels=c("m","f"))
psycho$GHQ <- factor(psycho$GHQ, levels=c(0,1), labels=c("low","high"))
psycho$n.not <- psycho$n.total - psycho$n.taking
contrasts(psycho$age.group) <- contr.poly(psycho$mean.age[1:5])
head(psycho)

pulmonary <- read.table(hh("datasets/pulmonary.dat"), header=TRUE,
                        row.names=NULL)
names(pulmonary)[3] <- "FVC"
names(pulmonary)[1] <- "smoker"
pulmonary$smoker <- factor(pulmonary$smoker, levels=pulmonary$smoker)
row.names(pulmonary) <- pulmonary$smoker
head(pulmonary)

pulse <- read.table(hh("datasets/pulse.dat"), header=TRUE)
names(pulse) <- c("task","pulse")
pulse$task <- factor(pulse$task)
head(pulse)

radioact                <- read.table(hh("datasets/radioact.dat"), header=TRUE)                 ; head(radioact                )

rent <- read.table(hh("datasets/rent.dat"), header=FALSE,   ## Weisberg's file alr162
                   col.names=c("rnt.alf","rnt.till", "cow.dens","prop.past","lime"))
rent$lime <- factor(rent$lime, labels=c("no.lime","lime"))
contrasts(rent$lime) <- contr.helmert(2)
rent$alf.till <- rent$rnt.alf / rent$rnt.till
head(rent)

retard                  <- read.table(hh("datasets/retard.dat"), header=TRUE)                   ; head(retard                  )

rhiz.alfalfa <- read.table(hh("datasets/rhiz1-alfalfa.dat"), header=TRUE)
rhiz.alfalfa$comb <- factor(rhiz.alfalfa$comb,
                            labels=c("alfalfa","alfalfa+clover"))
rhiz.alfalfa$strain <-
  factor(rhiz.alfalfa$strain,
         labels=c('3DOa1','3DOa7','3DOa10','3DOa12','3DOa15','a.comp'))
rhiz.alfalfa$Npg <- rhiz.alfalfa$nitro / rhiz.alfalfa$weight
head(rhiz.alfalfa)

rhiz.clover <- read.table(hh("datasets/rhiz3-clover.dat"), header=TRUE)
rhiz.clover$comb <- factor(rhiz.clover$comb,
                           labels=c("clover","clover+alfalfa"))
rhiz.clover$strain <-
  factor(rhiz.clover$strain,
         labels=c('3DOk1','3DOk5','3DOk4','3DOk7','3DOk13','k.comp'))
rhiz.clover$Npg <- rhiz.clover$nitro / rhiz.clover$weight
head(rhiz.clover)

rhizobium1              <- read.table(hh("datasets/rhizobium1.dat"), header=TRUE)               ; head(rhizobium1              )
rhizobium3              <- read.table(hh("datasets/rhizobium3.dat"), header=TRUE)               ; head(rhizobium3              )
salary                  <- read.table(hh("datasets/salary.dat"), header=TRUE, na.strings="*")                   ; head(salary                  )

if.R(r={
  salinity <- read.fwf(hh("datasets/salinity.dat"),
                       width=c(5,8,8),
                       col.names=c("1", "2", "3"), check.names=FALSE)
  salinity <- stack(salinity)
  names(salinity) <- c("salinity","body")
}, s={
  salinity <- read.table(hh("datasets/salinity.dat"),
                         sep=c(1,9,16),
                         col.names=c("1", "2", "3"))
  salinity <- data.frame(salinity=unlist(salinity),
                         body=rep(names(salinity), each=nrow(salinity)))
})
salinity <- salinity[!is.na(salinity$salinity),]
row.names(salinity) <- if.R(r=NULL,
                            s=1:nrow(salinity))
head(salinity)

## salk                    <- read.table(hh("datasets/salk.dat"), header=TRUE)                     ; head(salk                    )
salk <- read.table(hh("datasets/salk.dat"),
                   col.names=c("age", "vaccine", "paralyze", "Freq"))
salk$age <- ordered(salk$age,
                    labels=c("0-4", "5-9", "10-14", "15-19", "20-39", "40+"))
if.R(r={
  require(vcd)
  structable(~ age + vaccine + paralyze, data=salk,  direction=c("v","v","h"),
             main="Recommended display---specified, paralyze split last")
}, s={
    tmp <- tapply(salk$Freq, salk[, c(2,3,1)], c)
    names(dimnames(tmp)) <- c("vaccine","paralyze", "age")
    print(as.table(tmp))
    rm(tmp)
})
     

seeding                 <- read.table(hh("datasets/seeding.dat"), header=TRUE)                  ; head(seeding                 )

if.R(r={
  selfexam <- read.fwf(hh("datasets/selfexam.dat"),
                       widths=c(13,10,15,5),
                       row.names=1,
                       skip=2, header=FALSE)
  names(selfexam) <- read.table(hh("datasets/selfexam.dat"),
                                skip=1, nrows=1, as.is=TRUE)[-1]
}, s={
  selfexam <- read.table(hh("datasets/selfexam.dat"),
                         sep=c(1,14,24,39), skip=1, header=TRUE)
})
row.names(selfexam) <- gsub(" ", "", row.names(selfexam))
selfexam <- as.table(data.matrix(selfexam))
names(dimnames(selfexam)) <- c("age","frequency")
selfexam

shipment                <- read.table(hh("datasets/shipment.dat"), header=TRUE)                 ; head(shipment                )
sickle                  <- read.table(hh("datasets/sickle.dat"), header=FALSE, col.names=c("type","Hb.level"))                   ; head(sickle                  )
## simpson.datatable       <- read.table(hh("datasets/simpson.datatable"), header=TRUE)            ; head(simpson.datatable       )

skateslc                <- read.table(hh("datasets/skateslc.dat"), header=TRUE, skip=6)
skateslc$Skater <- factor(skateslc$Skater, labels=c("Hughes", "Slutskaya", "Kwan", "Cohen", "Suguri"))
head(skateslc)

smokers <- read.table(hh("datasets/smokers.dat"), header=FALSE, col.names=c("age","smoker","dead","Freq"))
smokers$smoker <- ordered(smokers$smoker, levels=c("yes","no"))
smokers$dead <- ordered(smokers$dead, levels=c("yes","no"))
##Appleton:1996
if.R(r={
  require(vcd)
  structable(~ smoker + age + dead, data=smokers,  direction=c("h","v","v"))
}, s={
  tmp <- tapply(smokers$Freq, smokers[, c(2,3,1)], c)
  names(dimnames(tmp)) <- c("smoker","dead", "age")
  print(as.table(tmp))
  rm(tmp)
})
     
spacshu                 <- read.table(hh("datasets/spacshu.dat"), header=FALSE, col.names=c("tempF", "damage"))                  ; head(spacshu                 )
spindle                 <- read.table(hh("datasets/spindle.dat"), header=TRUE)                  ; head(spindle                 )
sprint                  <- read.table(hh("datasets/sprint.dat"), header=TRUE)                   ; head(sprint                  )
stopdist                <- read.table(hh("datasets/stopdist.dat"), header=TRUE)                 ; head(stopdist                )
surface                 <- read.table(hh("datasets/surface.dat"), header=TRUE)                  ; head(surface                 )

## tablet1                 <- read.table(hh("datasets/tablet1.dat"), header=TRUE)                  ; head(tablet1                 )
tablet1 <- read.table(hh("datasets/tablet1.dat"), col.names=LETTERS[1:4])
if.R(r={
  tablet1 <- stack(tablet1)
  names(tablet1) <- c("time","tablet")
}, s={
  tablet1 <- data.frame(time=unlist(tablet1),
                        tablet=rep(names(tablet1), each=nrow(tablet1)))
})
head(tablet1)

teachers                <- read.table(hh("datasets/teachers.dat"), header=FALSE, col.names=c("English","Greek"))                 ; head(teachers                )

testing <- data.frame(strength=scan(hh("datasets/testing.dat")),
                      breaker=factor(rep(c(1,1,2,2,3,3), 6)),
                      gauger=factor(rep(c(1,2,3), c(12,12,12))))
head(testing)

testscore <- read.table(hh("datasets/testscore.dat"), header=TRUE)
testscore$sex       <- factor(testscore$sex,
                              labels=c("female","male"))
testscore$grade     <- ordered(testscore$grade)
testscore$standing  <- factor(testscore$standing,
                              labels=c("good", "average", "poor"))
testscore$order     <- factor(testscore$order,
                              labels=c("high", "medium", "low"))
head(testscore )

tires <- read.table(hh("datasets/tires.dat"), header=TRUE)
tires$car      <- factor(tires$car)
tires$position <- factor(tires$position)
tires$brand    <- factor(tires$brand)
head(tires)

tongue <- read.table(hh("datasets/tongue.dat"), header=TRUE)
head(tongue)

tser.mystery.X <- scan(hh("datasets/tser.mystery.X.dat"))
tser.mystery.X <- if.R(r=ts(tser.mystery.X), s=rts(tser.mystery.X))
ts(tser.mystery.X[1:12])

tser.mystery.Y <- scan(hh("datasets/tser.mystery.Y.dat"))
tser.mystery.Y <- if.R(r=ts(tser.mystery.Y), s=rts(tser.mystery.Y))
ts(tser.mystery.Y[1:12])

tser.mystery.Z <- scan(hh("datasets/tser.mystery.Z.dat"))
tser.mystery.Z <- if.R(r=ts(tser.mystery.Z), s=rts(tser.mystery.Z))
ts(tser.mystery.Z[1:12])

tsq <- scan(hh("datasets/tsq.dat"))
tsq <- if.R(r=ts(tsq), s=rts(tsq, units="days"))
ts(tsq[1:12])

## turkey <- read.table(hh("datasets/turkey.dat"), header=TRUE) ; head(turkey)
turkey <- read.table(hh("datasets/turkey.dat"), header=FALSE, col.names=c("diet","wt.gain"))
turkey$diet <- factor(turkey$diet,
                      labels=c("control","A1","A2","B1","B2"))
head(turkey)

## tv.txt <- read.table(hh("datasets/tv.txt"), header=TRUE)
## head(tv.txt)
## some country names have embedded blanks
tv <- if.R(r={
  read.fwf(hh("datasets/tv.dat"),
           width=c(22,6,7,7,4,2),
           strip.white=TRUE,
           row.names=1,
           na.strings="*",
           col.names=c("country","life.exp","ppl.per.tv","ppl.per.phys",
             "fem.life.exp","male.life.exp"))
}, s={
  read.table(hh("datasets/tv.dat"),
             sep=c(1,23,29,36,43,47),
             na.strings="*",
             col.names=c("country","life.exp","ppl.per.tv","ppl.per.phys",
               "fem.life.exp","male.life.exp"))
})
head(tv)

usair <- read.table(hh("datasets/usair.dat"),                    
                    col.names=c("SO2","temp","mfgfirms","popn",
                                "wind","precip","raindays"))
head(usair)

uscrime                 <- read.table(hh("datasets/uscrime.dat"), header=TRUE)                  ; head(uscrime                 )
vocab                   <- read.table(hh("datasets/vocab.dat"), header=FALSE, col.names="score")                    ; head(vocab                   )

vulcan <- read.table(hh("datasets/vulcan.dat"), header=TRUE)
vulcan$filler <- factor(vulcan$filler)
vulcan$pretreat <- factor(vulcan$pretreat)
vulcan$raw <- factor(vulcan$raw)
position(vulcan$pretreat) <- c(2,3,4)
position(vulcan$raw) <- (1:4)+.5
head(vulcan)

washday                 <- read.table(hh("datasets/washday.dat"), header=TRUE)                  ; head(washday                 )
water                   <- read.table(hh("datasets/water.dat"), header=TRUE)                    ; head(water                   )

weightloss <- read.table(hh("datasets/weightloss.dat"), header=TRUE)
weightloss <- data.frame(loss=unlist(weightloss),
                         group=rep(names(weightloss), rep(10,5)))
head(weightloss )

weld                    <- read.table(hh("datasets/weld.dat"), header=TRUE)                     ; head(weld                    )
wheat                   <- read.table(hh("datasets/wheat.dat"), header=TRUE)                    ; head(wheat                   )
wool                    <- read.table(hh("datasets/wool.dat"), header=TRUE)                     ; head(wool                    )

workstation <- read.table(hh("datasets/workstation.dat"), header=TRUE)
workstation$method <- factor(workstation$method)
workstation$station <- factor(workstation$station)
head(workstation)

yates                   <- read.table(hh("datasets/yates.dat"), header=TRUE)                    ; head(yates                   )

yatesppl <- read.table(hh("datasets/yatesppl.dat"), header=TRUE)
yatesppl$blocks   <- factor(yatesppl$blocks  )
yatesppl$plots    <- factor(yatesppl$plots   )
yatesppl$subplots <- factor(yatesppl$subplots)
yatesppl$variety  <- factor(yatesppl$variety )
yatesppl$nitrogen <- factor(yatesppl$nitrogen)
head(yatesppl)

## lft.asat <- read.csv(hh("/h2/datasets/lft.asat.dat"), header=TRUE);  head(lft.asat) redundant
## AudiencePercent is originally in rda form.


if.R(r={},
     s={
       if (exists("as.table", where=1))
         rm(as.table)
     })




## likert paper datasets.  Not part of HH book.

AudiencePercent <-
structure(c(0.9, 1.7, 5.5, 2, 2.7, 3.2, 48.1, 60.9, 67.6, 64, 
82.4, 62.5, 31.8, 31.2, 11.1, 14, 8.2, 27.4, 19.2, 6.2, 15.8, 
20, 6.7, 6.9), .Dim = c(6L, 4L), .Dimnames = structure(list(Brands = c("A", 
"B", "C", "D", "E", "F"), Age = c("20 & under", "21-35", "36-50", 
"51+")), .Names = c("Brands", "Age")))

NZScienceTeaching <-
structure(list(`Interest in Science` = structure(c(1, 2, 3, 4, 
5, 9, 2, 6, 7, 16, 32, 38, 17, 19, 19, 31, 33, 30, 51, 40, 50, 
33, 23, 20, 25, 32, 21, 16, 7, 2), .Dim = c(6L, 5L), .Dimnames = list(
    c("Science is really interesting", "I'm glad I decided to take science subjects this year", 
    "High school has increased my interest in science", "I was interested in science even before I started high school", 
    "My science classes are often taught in a boring way", "Science is mostly just about learning facts"
    ), c("Strongly disagree", "Disagree", "Neutral", "Agree", 
    "Strongly agree"))), `Tertiary Study` = structure(c(5, 6, 
10, 18, 17, 15, 20, 28, 33, 37, 28, 37, 30, 30, 28, 12, 14, 12, 
12, 5), .Dim = 4:5, .Dimnames = list(c("I feel overwhelmed by all the options", 
"I wish there were more people I could talk to", "I worry that I'm not making good choices", 
"I've been given advice or information that wasn't helpful"), 
    c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"
    )))), .Names = c("Interest in Science", "Tertiary Study"), class = c("listOfNamedMatrices", 
"list"))

PoorChildren <-
structure(list(Extremely.poor.NWP = c(606352, 1001894, 998587, 
533108, 406372, 391888), Moderately.poor.NWP = c(354007, 641736, 
629566, 308569, 242874, 197773), `Moderately.poor.1+WP` = c(930909, 
1548697, 1417142, 637017, 457117, 348115), `Extremely.poor.1+WP` = c(337828, 
566705, 535242, 251701, 197478, 183158)), .Names = c("Extremely.poor.NWP", 
"Moderately.poor.NWP", "Moderately.poor.1+WP", "Extremely.poor.1+WP"
), row.names = c("10 or less", "10 to 15", "15 to 20", "20 to 25", 
"25 to 30", "30 or more"), class = "data.frame")

ProfChal <-
structure(list(` ` = structure(c(4, 21, 27, 230, 283), .Dim = c(1L, 
5L), .Dimnames = list("All Survey Responses", c("Strongly Disagree", 
"Disagree", "No Opinion", "Agree", "Strongly Agree"))), `Employment sector` = structure(c(0, 
0, 2, 0, 2, 5, 11, 3, 0, 2, 8, 5, 5, 2, 5, 78, 88, 34, 15, 15, 
162, 72, 27, 11, 10), .Dim = c(5L, 5L), .Dimnames = list(c("Academic (nonstudent)", 
"Business and industry", "Federal, state, and local government", 
"Private consultant/self-employed", "Other (including retired, students, not employed, etc.)"
), c("Strongly Disagree", "Disagree", "No Opinion", "Agree", 
"Strongly Agree"))), Race = structure(c(4, 0, 0, 0, 11, 4, 1, 
2, 18, 4, 2, 1, 167, 49, 3, 6, 200, 65, 4, 8), .Dim = 4:5, .Dimnames = list(
    c("White", "Asian", "Black or African American", "Other"), 
    c("Strongly Disagree", "Disagree", "No Opinion", "Agree", 
    "Strongly Agree"))), Education = structure(c(2, 2, 12, 9, 
10, 17, 86, 143, 65, 217), .Dim = c(2L, 5L), .Dimnames = list(
    c("Associate's and Bachelor's", "Master's and Above"), c("Strongly Disagree", 
    "Disagree", "No Opinion", "Agree", "Strongly Agree"))), Gender = structure(c(3, 
1, 12, 7, 15, 12, 146, 78, 180, 102), .Dim = c(2L, 5L), .Dimnames = list(
    c("Male", "Female"), c("Strongly Disagree", "Disagree", "No Opinion", 
    "Agree", "Strongly Agree"))), `Attitude\ntoward\nProfessional\nRecognition` = structure(c(12, 
0, 9, 8, 30, 8, 156, 81, 156, 105), .Dim = c(2L, 5L), .Dimnames = list(
    c("Not Important", "Important"), c("Strongly Disagree", "Disagree", 
    "No Opinion", "Agree", "Strongly Agree")))), .Names = c(" ", 
"Employment sector", "Race", "Education", "Gender", "Attitude\ntoward\nProfessional\nRecognition"
), class = c("listOfNamedMatrices", "list"))

ProfDiv <-
structure(c(12, 18, 24, 39, 24, 9, 3, 2, 3, 8, 18, 19, 12, 2, 
55, 45, 43, 30, 42, 60, 73, 31, 34, 25, 13, 15, 19, 22), .Dim = c(7L, 
4L), .Dimnames = structure(list(Year = c("1935", "1934", "1933", 
"1932", "1931", "1930", "1929"), `Profit-and-Dividend` = c("UnProf-NoDiv", 
"UnProf-Div", "Prof-Div", "Prof-NoDiv")), .Names = c("Year", 
"Profit-and-Dividend")))

SFF8121 <-
structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 8, 0, 0, 0, 0, 17, 8, 0, 0, 9, 0, 0, 17, 0, 8, 58, 25, 8, 
8, 8, 36, 25, 42, 33, 42, 50, 42, 58, 83, 92, 92, 55, 75, 50, 
50, 58, 42, 1, 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 3, 3, 3, 2, 3, 4, 
2, 4, 2, 4, 3, 14, 10, 8, 10, 9, 13, 12, 11, 8, 12, 11, 52, 43, 
40, 39, 36, 37, 39, 38, 42, 39, 38, 31, 43, 48, 49, 51, 43, 45, 
45, 47, 43, 46), .Dim = c(11L, 5L, 2L), .Dimnames = structure(list(
    Question = c("1. I came well prepared for class.", "2. The instructor clearly explained the\neducational objectives of this course.", 
    "3. The instructor was well organized and\nprepared for class.", 
    "4. The instructor was conscientious in meeting\nclass and office hour responsibilities.", 
    "5. The instructor promoted a classroom\natmosphere in which I felt free to\nask questions.", 
    "6. The instructor provided useful feedback\nabout exams, projects, and assignments.", 
    "7. So far, the instructor has applied grading\npolicies fairly.", 
    "8. The instructor taught this course well.", "9. The course content was consistent\nwith the educational objectives of\nthis course.", 
    "10. The course increased my ability to\nanalyze and critically evaluate\nideas, arguments, and points of view.", 
    "11. I learned a great deal in this course."), ResponseLevel = c("Strongly Disagree", 
    "Disagree", "Neutral", "Agree", "Strongly Agree"), SDCLU = c("Stat 8121 --- Statistical Computing", 
    "All Graduate Courses")), .Names = c("Question", "ResponseLevel", 
"SDCLU")))
