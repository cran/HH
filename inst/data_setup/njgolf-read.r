njgolf2 <-
  read.table(hh("datasets/njgolf.dat"),
             header=F,
             na.strings="-999",
             col.names=
             c("id","index","lprice",
               "oprice","sprice","grid","subsect",
               "taxes","taxyr","lotsize","primsch","intsch",
               "highsch","agent","floodp","waterf","age",
               "stories","floor","style","firepl",
               "basemt","pool","garage","allrms",
               "beds","baths","lrarea",
               "drarea","kitarea",
               "famarea","den","study","masarea",
               "secarea","area3","area4","area5",
               "range","dwasher","washer","secsys",
               "micro","rtop","disposal","dryer",
               "freezer","compact","walloven","refrig",
               "patio","deck","fence","fuel",
               "ac","heat","hwater","water",
               "sewer","unknown","amps","assvalue","cfee",
               "l.ymd",   #"lyear",  "lmonth",  "lday",
               "ex.ymd",  #"exyear", "exmonth", "exday",
               "s.ymd",   #"syear",  "smonth",  "sday",
               "set.ymd", #"setyear","setmonth","setday",
               "golf","culdesac","unrate","greenmm","gatemm"),
             row.names="id"
             )
## table(njgolf2$lotsize, njgolf2$style)

attach(njgolf2)
lotsizef <- factor(match(lotsize, "CON", 0) +
                   2*match(lotsize, "TH", 0) +
                   3*is.na(lotsize),
                   levels=c(0,1,2,3),
                   labels=c("house","con","th","NA"))
options(warn=-1) ## suppress warning message in next line
lotsize <- as.numeric(as.character(lotsize))
options(warn=0)  ## restore check for warning messages
lotsize[is.na(lotsize)] <- 0
own <- as.character(lotsizef)

## table(lotsize, na.include(lotsizef), exclude=NULL)

##*** various measures of gate and green    ******
 gate <- gatemm * .002982575
 green <- greenmm * .002982575
 invgate <- 1/gate
 linvgate <- log(invgate)
 invgate2 <- invgate * invgate

##*****  make some logs    ******
 loglp <- log(lprice)
 logsp <- log(sprice)
 loglot <- log(lotsize)
 logage <- log(age)
 logbeds <- log(beds)
 logbaths <- log(baths)
 logdra <- log(drarea)
 logkita <- log(kitarea)
 logstor <- log(stories)
 lograte <- log(unrate)

##*****  compute sequential months variable   ******
## these dates() are correct, no 30 day approximations.
if.R(s={
  syear <-  substring(s.ymd, 1, 2)
  smonth <- substring(s.ymd, 3, 4)
  sday <-   substring(s.ymd, 5, 6)
  sdate <- dates(paste(syear,smonth,sday, sep="/"), format="yy/mm/dd",
                 origin=c(month=2, day=29, year=1992))
  time.months <- as.numeric(sdate)/30  ## originally: time
},r={
  sdate <- as.Date(paste("19",s.ymd,sep=""), "%Y%m%d")
  sdate.origin <- as.Date("1992/2/29", "%Y/%m/%d")
  time.months <- (as.numeric(sdate)-as.numeric(sdate.origin))/30  ## originally: time
})
logtime <- log(time.months)

##**** compute primary school groups  ******
## I am creating the factor directly, not the set of dummy variables
primf <- primsch
primf[primsch > 3] <- 1
primf <- factor(primf, labels=c("other",2,3))
contrasts(primf) <- contr.treatment(3)

##***** create dummies for house style   *****
## I am creating the factor directly, not the set of dummy variables
stylef <- style
stylef[as.logical(match(style, 4:6, 0))] <- 2
stylef[as.logical(match(style, c(2,3,7,8), 0))] <- 3
stylef <- factor(stylef, labels=c("colonial","condos","ostyles"))

##*****   compute appliances variables   ******
noappl = range + dwasher + washer + micro + rtop +
  disposal + dryer + freezer + walloven + refrig
noappl[is.na(noappl)] <- -.1
logappl = log(noappl+.2)

##***** compute basement types     ***************
basemtf <- factor(basemt, labels=c("fullbase","partbase","nobase"))
nobase <- factor(basemtf=="nobase")

##*****   compute garage variables   *******
gar12 <- (garage < 2)


##********     compute listing period if same year  ************
if.R(s={
  lyear <-  substring(l.ymd, 1, 2)
  lmonth <- substring(l.ymd, 3, 4)
  lday <-   substring(l.ymd, 5, 6)
  ldate <- dates(paste(lyear,lmonth,lday, sep="/"), format="yy/mm/dd",
                 origin=c(month=2, day=29, year=1992))

  exyear <-  substring(ex.ymd, 1, 2)
  exmonth <- substring(ex.ymd, 3, 4)
  exday <-   substring(ex.ymd, 5, 6)
  exdate <- dates(paste(exyear,exmonth,exday, sep="/"), format="yy/mm/dd",
                  origin=c(month=2, day=29, year=1992))

  setyear <-  substring(set.ymd, 1, 2)
  setmonth <- substring(set.ymd, 3, 4)
  setday <-   substring(set.ymd, 5, 6)
  setdate <- dates(paste(setyear,setmonth,setday, sep="/"), format="yy/mm/dd",
                  origin=c(month=2, day=29, year=1992))
  remove(list=c(
           "exyear", "lyear",  "setyear", "syear",
           "exmonth", "lmonth",  "setmonth", "smonth",
           "exday", "lday",  "setday", "sday"))
},r={
  ldate <- as.Date(paste("19",l.ymd,sep=""), "%Y%m%d")
  exdate <- as.Date(paste("19",ex.ymd,sep=""), "%Y%m%d")
  setdate <- as.Date(paste("19",set.ymd,sep=""), "%Y%m%d")
})
listp <- as.numeric(exdate-ldate)

##***** create days from sale to expiration    *******
expdays <- as.numeric(exdate-sdate)


##***** create tom - lp start to date of sale  *******
tom <- as.numeric(sdate-ldate)

## expdays[expdays==0] <- NA  ## rmh doesn't understand NA here
## listp[listp==0] <- NA

expper = expdays/listp
invexp = 1/(expdays+.1)
logiexp = log(invexp)
tomper = tom/listp
rellp = listp/180
reltom = tom/rellp

longlp <- rellp
longlp[longlp > 1] <- 1

shortlp = 1 - longlp

##******        calculate some interactions with tom      ********
longtom = tom * longlp
shorttom = tom * shortlp
tomrlp = tom * rellp
tomlp = tom * listp

##*****    compute price concession variables   **********
pdisc = (lprice - sprice) / sprice
logpdisc = log(pdisc+.1)
lpsp = lprice/sprice

detach("njgolf2")

### bring it all into a dataframe
tmp <- data.frame(lotsize, lotsizef, own, gate, green,
                  invgate, linvgate,
                  invgate2, loglp, logsp, loglot, logage, logbeds,
                  logbaths, logdra, logkita, logstor, lograte, sdate,
                  time.months, logtime, primf, stylef, noappl,
                  logappl, basemtf, nobase, gar12, ldate, exdate, setdate,
                  listp,
                  expdays, tom, expper, invexp, logiexp, tomper, rellp,
                  reltom, longlp, shortlp, longtom, shorttom, tomrlp,
                  tomlp, pdisc, logpdisc, lpsp)
## make sure the njgolf gets the redefined version if the name is the same
njgolf <- njgolf2
for (i in names(tmp)) njgolf[[i]] <- tmp[[i]]

njgolf$s.ymd   <- njgolf$sday    <- njgolf$smonth  <- njgolf$syear   <- NULL
njgolf$l.ymd   <- njgolf$lday    <- njgolf$lmonth  <- njgolf$lyear   <- NULL
njgolf$ex.ymd  <- njgolf$exday   <- njgolf$exmonth <- njgolf$exyear  <- NULL
njgolf$set.ymd <- njgolf$setday  <- njgolf$setmonth<- njgolf$setyear <- NULL

remove(list=names(tmp))
if.R(r={
  rm(i, sdate.origin, tmp, njgolf2)
}, s={
  rm(tmp, njgolf2)
})
