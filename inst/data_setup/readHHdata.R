Design_2.8_2_full       <- read.table(hh("datasets/2.8-2-full.dat"), header=TRUE)               ; head(Design_2.8_2_full       )
Design_2.8_2            <- read.table(hh("datasets/2.8-2.dat"), header=FALSE, col.names=c("blocks","trt"))                   ; head(Design_2.8_2            )
abrasion                <- read.table(hh("datasets/abrasion.dat"), header=TRUE)                 ; head(abrasion                )

acacia <- matrix(c(2,13,10,3), nrow=2, byrow=TRUE,
                 dimnames=list(
                   Species=c("A","B"),
                   invasion=c("Not.Inv","Invaded")))
acacia <- as.table(acacia)

aedotplot               <- read.table(hh("datasets/aedotplot.dat"), header=TRUE, sep=",")                ; head(aedotplot               )
animal                  <- read.table(hh("datasets/animal.dat"), header=TRUE)                   ; head(animal                  )
anneal                  <- read.table(hh("datasets/anneal.dat"), header=TRUE)                   ; head(anneal                  )
apple                   <- read.table(hh("datasets/apple.dat"), header=TRUE)                    ; head(apple                   )
ara                     <- read.table(hh("datasets/ara.dat"), header=TRUE)                      ; head(ara                     )
balance                 <- read.table(hh("datasets/balance.dat"), header=FALSE, col.names=c("sway","age"))                  ; head(balance                 )
barleyp                 <- read.table(hh("datasets/barleyp.dat"), header=TRUE)                  ; head(barleyp                 )
batch                   <- read.table(hh("datasets/batch.dat"), header=TRUE)                    ; head(batch                   )
bean                    <- read.table(hh("datasets/bean.dat"), header=TRUE)                     ; head(bean                    )
birthweight             <- read.table(hh("datasets/birthweight.dat"), header=TRUE);  birthweight$gender <- factor(birthweight$gender, labels=c("F","M"))            ; head(birthweight             )
blood                   <- read.table(hh("datasets/blood.dat"), header=FALSE, col.names=c("times","diets")) ; blood$diets <- factor(blood$diets, labels=LETTERS[1:4])                   ; head(blood                   )
blyth                   <- read.table(hh("datasets/blyth.dat"), header=TRUE)                    ; head(blyth                   )
breast                  <- read.table(hh("datasets/breast.dat"), header=TRUE)                   ; head(breast                  )
budworm                 <- read.table(hh("datasets/budworm.dat"), header=TRUE)                  ; head(budworm                 )

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

cc135                   <- read.table(hh("datasets/cc135.dat"), header=TRUE)                    ; head(cc135                   )

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
rm(current.levels)
## contrasts(cc176$n.treats)
contrasts(cc176$n.treats) <-
       contr.poly(3, scores=c(1,3,6))
## contrasts(cc176$n.treats)
head(cc176                   )

cement                  <- read.table(hh("datasets/cement.dat"), header=FALSE, col.names=c("heat","conc1","conc2","conc3","conc4"))                   ; head(cement                  ) ## datasets-maybe/NIST.dat/regb/cement.txt
census4                 <- read.table(hh("datasets/census4.dat"), header=FALSE)                  ; head(census4                 ) ## ?
cereals                 <- read.table(hh("datasets/cereals.dat"), header=TRUE)                  ; head(cereals                 )
## cereals.info            <- read.table(hh("datasets/cereals.info"), header=TRUE)                 ; head(cereals.info            )
chimp                   <- read.table(hh("datasets/chimp.dat"), header=TRUE)                    ; head(chimp                   )
circuit                 <- read.table(hh("datasets/circuit.dat"), header=TRUE)                  ; head(circuit                 )
co2                     <- datasets::co2                     ; head(co2                     ) ## Use the builtin R data set
concord                 <- read.table(hh("datasets/concord.dat"), header=TRUE)                  ; head(concord                 )
crash                   <- read.table(hh("datasets/crash.dat"), header=TRUE)                    ; head(crash                   )
## crash.datatable         <- read.table(hh("datasets/crash.datatable"), header=TRUE)              ; head(crash.datatable         )
crime                   <- as.table(data.matrix(read.table(hh("datasets/crime.dat"), header=TRUE)))                    ; head(crime                   )
darwin                  <- read.table(hh("datasets/darwin.dat"), header=FALSE, col.names=c("cross","self"))                   ; head(darwin                  )
diamond                 <- read.table(hh("datasets/diamond.dat"), header=TRUE)                  ; head(diamond                 )
display                 <- read.table(hh("datasets/display.dat"), header=TRUE)                  ; head(display                 )
distress                <- read.table(hh("datasets/distress.dat"), header=FALSE, col.names=c("birthwt","outcome"))                 ; head(distress                )

## draft70mn               <- read.table(hh("datasets/draft70mn.dat"), header=TRUE)                ; head(draft70mn               )
draft70mn <- read.fwf(hh("datasets/draft70mn.dat"), header=FALSE,
                      widths=c(5,rep(4,11)),
                      col.names=month.abb)
head(draft70mn               )
draft <- stack(draft70mn)
names(draft) <- c("rank","month")
draft$monthOrd <- ordered(draft$month, month.abb)
head(draft               )

drunk <- read.table(hh("datasets/drunk.dat"),
                    col.names=c('0-29','30-39','40-49','50-59','>=60'),
                    row.names=c("males","females"),
                    check.names=FALSE)
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
employM16 <- ts(t(employM16), start=c(1948,1), frequency=12)
ts(employM16[1:24], start=c(1948,1), frequency=12)

energy <- read.table(hh("datasets/energy.dat"), header=TRUE, sep=",")
energy$Wood <- ordered(energy$Wood,
                       levels=c(
                         "Osage Orange",
                         "Red Oak",
                         "Black Walnut",
                          "White Pine"))
energy$Stove <- factor(energy$Stove, labels=c("A","B","C"))
## energy <- energy[order(energy$Moist, energy$Wood, energy$Kind),]
head(energy)

esr                     <- read.table(hh("datasets/esr.dat"), header=FALSE, col.names=c("fibrin","gammaglob","ESR"))                      ; head(esr                     )

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

feed                    <- read.table(hh("datasets/feed.dat"), header=TRUE)                     ; head(feed                    )

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
contrasts(gum$Time) <- contr.poly(3, c(20,30,50))
gum$subject <- factor(rep(1:8, 6))
gum$pH <-  unlist(gum.in[2:4])
rm(gum.in)
head(gum)

gunload                 <- read.table(hh("datasets/gunload.dat"), header=TRUE)                  ; head(gunload                 )
har1                    <- read.table(hh("datasets/har1.dat"), header=TRUE)                     ; head(har1                    )

har2                    <- read.table(hh("datasets/har2.dat"), header=TRUE, fill=TRUE)
har2 <- stack(har2)
names(har2) <- c("weight", "treatment")
levels(har2$treatment) <- c("control","treated")
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
iceskate <- scan(hh("datasets/iceskate-w3.dat"), what="", multi.line=T)
iceskate <- matrix(iceskate, 23, 28, byrow=T)
iceskate <- data.frame(country=iceskate[,2],
                       technical=iceskate[,c(7:15)],
                       presentation=iceskate[,c(20:28)],
                       row.names=paste(iceskate[,3], iceskate[,4]))
for (i in 2:19) iceskate[,i] <- as.numeric(as.character(iceskate[,i]))
rm(i)
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
structable( ~ center + invas + vent, direction=c("v","v","h"), data=intubate)

ironpot                 <- read.table(hh("datasets/ironpot.dat"), header=TRUE)                  ; head(ironpot                 )
kangaroo                <- read.table(hh("datasets/kangaroo.dat"), header=TRUE)                 ; head(kangaroo                )
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
##longley                 <- read.table(hh("datasets/longley.dat"), header=FALSE)                  ; head(longley                 )
longley <- datasets::longley; head(longley)
lymph                   <- read.table(hh("datasets/lymph.dat"), header=TRUE)                    ; head(lymph                   )
maiz                    <- read.table(hh("datasets/maiz.dat"), header=TRUE)                     ; head(maiz                    )
manhours                <- read.table(hh("datasets/manhours.dat"), header=FALSE,
                                      col.names=c("manhours", "occupanc", "checkins", "svcdesk",
                                        "common", "wings", "berthing", "rooms"))                ; head(manhours                )
market                  <- read.table(hh("datasets/market.dat"), header=TRUE)                   ; head(market                  )
mice                    <- read.table(hh("datasets/mice.dat"), header=TRUE)                     ; head(mice                    )
mileage                 <- read.table(hh("datasets/mileage.dat"), header=TRUE)                  ; head(mileage                 )

mortality               <- read.fwf(hh("datasets/mortality.dat"), header=FALSE, col.names=c("x","birthweight","dead","alive"), row.names="birthweight", skip=3, widths=c(1,10,9,14))[,-1]
mortality <- as.table(data.matrix(mortality))
names(dimnames(mortality)) <- c("birthweight","outcome")
mortality

mpg                     <- read.table(hh("datasets/mpg.dat"), header=TRUE)                      ; head(mpg                     )
muscle                  <- read.table(hh("datasets/muscle.dat"), header=TRUE)                   ; head(muscle                  )
source("njgolf-read.r") ; head(njgolf)
normtemp                <- read.table(hh("datasets/normtemp.dat"), header=FALSE, col.names=c("BodyTempF","Gender","HeartRate"))
normtemp$Gender=factor(normtemp$Gender, labels=c("male","female"))
head(normtemp) ## Journal of Statistics Education (Shoemaker 1996)

notch <- read.table(hh("datasets/notch.dat"), header=FALSE,
                    col.names=c("energy","machine"))
notch$machine <- factor(notch$machine,
                        labels=c("Tinius1","Tinius2","Satec","Tokyo"))
head(notch)

## nottem                  <- read.table(hh("datasets/nottem.dat"), header=TRUE)                   ; head(nottem                  )
nottem <- datasets::nottem
oats                    <- read.table(hh("datasets/oats.dat"), header=TRUE)                     ; head(oats                    )
odoffna                 <- read.table(hh("datasets/odoffna.dat"), header=TRUE)                  ; head(odoffna                 )
operator                <- read.table(hh("datasets/operator.dat"), header=TRUE)                 ; head(operator                )

ozone <- read.table(hh("datasets/ozone.dat"), na.strings="-9999")
ozone <- ts(as.vector(t(ozone[,5:16])), start=c(1926,1), freq=12)
ts(ozone[1:36], start=c(1926,1), frequency=12)

paper                   <- read.table(hh("datasets/paper.dat"), header=FALSE, col.names=paste("material", 1:5, sep="."))
## From Hoaglin, Mosteller, Tukey,
## Fundamentals of Exploratory Analysis of Variance
## Wiley 1991
## page 375
paper <- data.frame(smoothness=unlist(paper),
                    material=factor(rep(1:5, rep(32,5))))
row.names(paper) <- NULL
head(paper)

## patient                 <- read.table(hh("datasets/patient.dat"), header=TRUE)                  ; head(patient                 )
tmp <- read.fwf(hh("datasets/patient.dat"),
                width=c(8,8,8,8,8),
                col.names=c("stomach","bronchus",
                    "colon","ovary","breast"))
patient <- stack(tmp)
names(patient) <- c("surv.time", "organ")
patient <- patient[!is.na(patient$surv.time),]
head(patient)


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
product <- ts(diff(product))
ts(product[1:20])

psycho <- read.table(hh("datasets/psycho.dat"), header=FALSE,
                     col.names=c("sex","age.group","mean.age",
                                 "GHQ","n.taking","n.total"))
psycho$sex <- factor(psycho$sex, levels=c(0,1), labels=c("m","f"))
psycho$GHQ <- factor(psycho$GHQ, levels=c(0,1), labels=c("low","high"))
psycho$n.not <- psycho$n.total - psycho$n.taking
contrasts(psycho$age.group) <- contr.poly(psycho$mean.age[1:5])
head(psycho)

pulmonary               <- read.table(hh("datasets/pulmonary.dat"), header=TRUE)                ; head(pulmonary               )
pulse                   <- read.table(hh("datasets/pulse.dat"), header=TRUE)                    ; head(pulse                   )
radioact                <- read.table(hh("datasets/radioact.dat"), header=TRUE)                 ; head(radioact                )

rent <- read.table(hh("datasets/rent.dat"), header=FALSE,   ## Weisberg's file alr162
                   col.names=c("rnt.alf","rnt.till", "cow.dens","prop.past","lime"))
rent$lime <- factor(rent$lime, labels=c("no.lime","lime"))
contrasts(rent$lime) <- contr.helmert(2)
rent$alf.till <- rent$rnt.alf / rent$rnt.till
head(rent)

retard                  <- read.table(hh("datasets/retard.dat"), header=TRUE)                   ; head(retard                  )
rhiz1_alfalfa           <- read.table(hh("datasets/rhiz1-alfalfa.dat"), header=TRUE)            ; head(rhiz1_alfalfa           )
rhiz3_clover            <- read.table(hh("datasets/rhiz3-clover.dat"), header=TRUE)             ; head(rhiz3_clover            )
rhizobium1              <- read.table(hh("datasets/rhizobium1.dat"), header=TRUE)               ; head(rhizobium1              )
rhizobium3              <- read.table(hh("datasets/rhizobium3.dat"), header=TRUE)               ; head(rhizobium3              )
salary                  <- read.table(hh("datasets/salary.dat"), header=TRUE, na.strings="*")                   ; head(salary                  )

salinity <- read.fwf(hh("datasets/salinity.dat"),
                     width=c(5,8,8),
                     col.names=c("1", "2", "3"), check.names=FALSE)
salinity <- stack(salinity)
names(salinity) <- c("salinity","body")
salinity <- salinity[!is.na(salinity$salinity),]
row.names(salinity) <- NULL
head(salinity)

## salk                    <- read.table(hh("datasets/salk.dat"), header=TRUE)                     ; head(salk                    )
salk <- read.table(hh("datasets/salk.dat"),
                   col.names=c("age", "vaccine", "paralyze", "Freq"))
salk$age <- ordered(salk$age,
                    labels=c("0-4", "5-9", "10-14", "15-19", "20-39", "40+"))
structable(~ age + vaccine + paralyze, data=salk,  direction=c("v","v","h"),
           main="Recommended display---specified, paralyze split last")


seeding                 <- read.table(hh("datasets/seeding.dat"), header=TRUE)                  ; head(seeding                 )

selfexam <- read.fwf(hh("datasets/selfexam.dat"),
                     widths=c(13,10,15,5),
                     row.names=1,
                     skip=2, header=FALSE)
names(selfexam) <- read.table(hh("datasets/selfexam.dat"),
                              skip=1, nrows=1, as.is=TRUE)[-1]
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
structable(~ smoker + age + dead, data=smokers,  direction=c("h","v","v"))

spacshu                 <- read.table(hh("datasets/spacshu.dat"), header=FALSE, col.names=c("tempF", "damage"))                  ; head(spacshu                 )
spindle                 <- read.table(hh("datasets/spindle.dat"), header=TRUE)                  ; head(spindle                 )
sprint                  <- read.table(hh("datasets/sprint.dat"), header=TRUE)                   ; head(sprint                  )
stopdist                <- read.table(hh("datasets/stopdist.dat"), header=TRUE)                 ; head(stopdist                )
surface                 <- read.table(hh("datasets/surface.dat"), header=TRUE)                  ; head(surface                 )

## tablet1                 <- read.table(hh("datasets/tablet1.dat"), header=TRUE)                  ; head(tablet1                 )
tablet1 <- read.table(hh("datasets/tablet1.dat"), col.names=LETTERS[1:4])
tablet1 <- stack(tablet1)
names(tablet1) <- c("time","tablet")
head(tablet1)

teachers                <- read.table(hh("datasets/teachers.dat"), header=FALSE, col.names=c("English","Greek"))                 ; head(teachers                )

testing <- data.frame(strength=scan(hh("datasets/testing.dat")),
                      breaker=factor(rep(c(1,1,2,2,3,3), 6)),
                      gauger=factor(rep(c(1,2,3), c(12,12,12))))
head(testing)

testscore               <- read.table(hh("datasets/testscore.dat"), header=TRUE)                ; head(testscore               )
tires                   <- read.table(hh("datasets/tires.dat"), header=TRUE)                    ; head(tires                   )
tongue                  <- read.table(hh("datasets/tongue.dat"), header=TRUE)                   ; head(tongue                  )
tser.mystery.X          <- ts(scan(hh("datasets/tser.mystery.X.dat")))           ; ts(tser.mystery.X[1:12])
tser.mystery.Y          <- ts(scan(hh("datasets/tser.mystery.Y.dat")))           ; ts(tser.mystery.Y[1:12])
tser.mystery.Z          <- ts(scan(hh("datasets/tser.mystery.Z.dat")))           ; ts(tser.mystery.Z[1:12])
tsq                     <- ts(scan(hh("datasets/tsq.dat")))                      ; ts(tsq[1:12])
## turkey                  <- read.table(hh("datasets/turkey.dat"), header=TRUE)                   ; head(turkey                  )
turkey <- read.table(hh("datasets/turkey.dat"), header=FALSE, col.names=c("diet","wt.gain"))
turkey$diet <- factor(turkey$diet,
                      labels=c("control","A1","A2","B1","B2"))
head(turkey)

## tv.txt                  <- read.table(hh("datasets/tv.txt"), header=TRUE)                       ; head(tv.txt                  )
tv <- read.fwf(hh("datasets/tv.dat"),
               width=c(22,6,7,7,4,2),
               strip.white=TRUE,
               row.names=1,
               na.strings="*",
               col.names=c("country","life.exp","ppl.per.tv","ppl.per.phys",
                 "fem.life.exp","male.life.exp"))
head(tv)

usair <- read.table(hh("datasets/usair.dat"),                    
                    col.names=c("SO2","temp","mfgfirms","popn",
                                "wind","precip","raindays"))
head(usair)

uscrime                 <- read.table(hh("datasets/uscrime.dat"), header=TRUE)                  ; head(uscrime                 )
vocab                   <- read.table(hh("datasets/vocab.dat"), header=FALSE, col.names="score")                    ; head(vocab                   )
vulcan                  <- read.table(hh("datasets/vulcan.dat"), header=TRUE)                   ; head(vulcan                  )
## vulcan0                 <- read.table(hh("datasets/vulcan0.dat"), header=TRUE)                  ; head(vulcan0                 )
washday                 <- read.table(hh("datasets/washday.dat"), header=TRUE)                  ; head(washday                 )
water                   <- read.table(hh("datasets/water.dat"), header=TRUE)                    ; head(water                   )
weightloss              <- read.table(hh("datasets/weightloss.dat"), header=TRUE)               ; head(weightloss              )
weld                    <- read.table(hh("datasets/weld.dat"), header=TRUE)                     ; head(weld                    )
wheat                   <- read.table(hh("datasets/wheat.dat"), header=TRUE)                    ; head(wheat                   )
wool                    <- read.table(hh("datasets/wool.dat"), header=TRUE)                     ; head(wool                    )
workstation             <- read.table(hh("datasets/workstation.dat"), header=TRUE)              ; head(workstation             )
yates                   <- read.table(hh("datasets/yates.dat"), header=TRUE)                    ; head(yates                   )
yatesppl                <- read.table(hh("datasets/yatesppl.dat"), header=TRUE)                 ; head(yatesppl                )
## lft.asat <- read.csv(hh("/h2/datasets/lft.asat.dat"), header=TRUE);  head(lft.asat) redundant
## AudiencePercent is originally in rda form.
