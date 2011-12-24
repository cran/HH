library(HH)

data(ProfChal)

## Table 1
ProfChal[2]

## Figure 1
plot.likert(ProfChal[[2]], xlab="Counts",             ## 8in x 3in  ## PC2C.pdf
            main="Is your job professionally challenging?")

## Figure 2
plot.likert(ProfChal[[2]], as.percent=TRUE, ## 8in x 3in  ## PC2Cpct.pdf
            xlab="Percent",
            main="Is your job professionally challenging?")

## Figure 3
plot.likert(ProfChal[[2]], as.percent=TRUE, ## 8in x 3in  ## PC2Cpctpo.pdf
            main="Is your job professionally challenging?",
            xlab="Percent",
            positive.order=TRUE)

## Table 3
tmp <- ProfChal[[2]]
rownames(tmp) <- abbreviate(rownames(tmp))
tmp
as.likert(tmp)

## Figure 4
ProfChalPctPlot <- ## 8in x 7in  ## ProfChal.pdf
plot.likert(ProfChal,
            as.percent=TRUE,    ## implies display Row Count Totals
            box.width=unit(.4,"cm"),
            strip.left.par=list(cex=.7, lines=5),
            xlab="Percent",
            main="Is your job professionally challenging?",
            main.middle=.65,
            positive.order=TRUE)
update(ProfChalPctPlot, par.settings=list(layout.widths=list(ylab.right=5)))
##ProfChalPctPlot


## Figure 5
## http://www.morst.govt.nz/Documents/publications/researchreports/Staying-in-Science-summary.pdf

## New Zealand students still taking science in Year 13.
## "Students’ interest in science, and views about secondary science teaching"
## "Students’ feelings about making tertiary study decisions"
data(NZScienceTeaching)
NZScienceTeaching
plot.likert(NZScienceTeaching, ## 10in x 5in  ## NZscienceteaching.pdf
            main="New Zealand Students Still Taking Science in Year 13",
            auto.key=list(between=.8, cex=.8),
            xlab="Percent",
            scales=list(y=list(cex=.9)),
            strip.left.par=list(cex=1.2, lines=1.3))


## Figure 6
## 9in x 7.5in   ## SFF8121.pdf
data(SFF8121)
as.MatrixList(SFF8121)
plot.likert(SFF8121, layout=c(2,1), between=list(x=1),
            scales=list(x=list(alternating=FALSE)),
            strip.left=FALSE,
            xlab="Percent",
            main="Student Feedback Forms, Spring 2010")


## Figure 7
## PopUSA1939-1979.pdf
## this needs a 17x8 window
data(USAge.table)
tmp <- USAge.table[75:1, 2:1, seq(80,40,-10)]
result <- plot.likert(tmp/1000000, strip.left=FALSE,
                      main="Population of United States (ages 0-74)",
                      xlab="Count in Millions",
                      sub="Look for the Baby Boom",
                      scales=list(
                        y=list(
                          limits=c(0,77),
                          at=seq(1,76,5),
                          labels=seq(0,75,5),
                          alternating=3,
                          tck=1),
                        x=list(alternating=FALSE)))
update(result, layout=c(5,1), between=list(x=.5))


## Figure 8a
## popUSA1979.pdf
## 7x8 window
USA79 <- USAge.table[75:1, 2:1, "1979"]/1000000
PL <- plot.likert(USA79,
                  main="Population of United States 1979 (ages 0-74)",
                  xlab="Count in Millions",
                  ylab="Age",
                  scales=list(
                    y=list(
                      limits=c(0,77),
                      at=seq(1,76,5),
                      labels=seq(0,75,5),
                      tck=.5))
                     )
PL
## Figure 8a
## popUSA1979pyr.pdf
as.pyramidLikert(PL)


## Figure 9
## top is historical
## bottom
## ProfitDividend.pdf
## 4.5in x 2.75in
data(ProfDiv)
plot.likert(ProfDiv, horizontal=FALSE, positive.order=FALSE,
            auto.key=list(reverse=TRUE, columns=1, space="right", size=4, padding.text=1.5),
            ylab="Year", xlab="Per Cent",
            main="Profit-and-Dividend Status of 348 Corportations in the United States\nfor the period from 1929 to 1935.",
            sub="Dun's Review, April 1938",
            par.settings=list(layout.widths=list(right.padding=2.5)))



## Figure 10
## AudiencePercent.pdf
## 7in x 4in
data(AudiencePercent)
plot.likert(AudiencePercent,
            positive.order=TRUE,
            auto.key=list(columns=4, between=1, between.columns=2),
            xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
            main="Brand A has the most even distribution of ages",
            scales=list(x=list(at=seq(-90,60,10), labels=as.vector(rbind("",seq(-80,60,20))))),     ## one or none of these scales() statements
            ##scales=list(x=list(at=seq(-100,75,25), labels=as.vector(rbind(seq(-100,75,25),"")))), ##
            col=brewer.pal(9, "Blues")[4:7])

## Figure 11
## screen shot from Tableau worksheet JSS_figures6-1.twbx
## TableauWorksheet.pdf

## Figure 12
## pdf file drawn by Tableau
## Figure12b.pdf


## Figure 13
## ProfChal6_131.pdf
## 9in x 9in
## ProfChal6_132.pdf
## 9in x 9in
data(ProfChal)
##
AA <- plot.likert(ProfChal[[1]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[1])
BB <- plot.likert(ProfChal[[2]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[2])
CC <- plot.likert(ProfChal[[3]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[3])
DD <- plot.likert(ProfChal[[4]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[4])
EE <- plot.likert(ProfChal[[5]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[5])
FF <- plot.likert(ProfChal[[6]], as.percent=TRUE, box.width=unit(.4,"cm"),
                  positive.order=TRUE, xlab="Percent", main=names(ProfChal)[6])
##
print(AA, more=TRUE,  split=c(1,1,1,3))
print(BB, more=TRUE,  split=c(1,2,1,3))
print(CC, more=FALSE,  split=c(1,3,1,3))
## ProfChal6_131.pdf
print(DD, more=TRUE,  split=c(1,1,1,3))
print(EE, more=TRUE,  split=c(1,2,1,3))
print(FF, more=FALSE, split=c(1,3,1,3))
## ProfChal6_132.pdf
