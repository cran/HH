## many examples of likert
## these were removed from ?likert to keep its size under control.


require(grid)
require(lattice)
require(latticeExtra)
require(HH)

data(ProfChal)  ## List of named matrices.  See below for discussion of the dataset.
ProfChal[[2]]

## Count plot
likert(ProfChal[[2]],
       main='Is your job professionally challenging?',
       sub="This plot looks better in a 9in x 4in window.")

## Percent plot calculated automatically from Count data
likert(ProfChal[[2]], as.percent=TRUE,
       main='Is your job professionally challenging?',
       sub="This plot looks better in a 9in x 4in window.")

## Examples with higher-dimensional arrays.
tmp2 <- array(1:12, dim=c(3,4), dimnames=list(B=LETTERS[3:5], C=letters[6:9]))
tmp3 <- array(1:24, dim=c(2,3,4),
              dimnames=list(A=letters[1:2], B=LETTERS[3:5], C=letters[6:9]))
tmp4 <- array(1:120, dim=5:2,
              dimnames=list(
                W=LETTERS[10:14],
                X=letters[6:9],
                Y=LETTERS[3:5],
                Z=letters[1:2]))

## positive.order=FALSE is the default.  With arrays
## the rownames within each item of an array are identical.

likert(tmp2)
likert(tmp2, ReferenceZero=2.5, main="same as previous graph")
dimnames(tmp2)[[2]] <-
    c("Disagree", "Neutral", "Agree Weakly", "Agree Strongly")
likert(tmp2, ReferenceZero=2,
    main="Agreement Levels:\none disagree and two agree levels")
dimnames(tmp2)[[2]] <-
    c("Disagree Strongly", "Neutral", "Agree Weakly", "Agree Strongly")
likert(tmp2, ReferenceZero=2, col=likertColor(5, 3)[-2],
       main="Agreement Levels:\none strongly disagree and two agree levels")

lik2 <- as.likert(tmp2)
plot(lik2, main="default")
plot(lik2, xTickLabelsPositive=FALSE,
     main="xTickLabelsPositive=FALSE")
plot(lik2,                            xlimEqualLeftRight=TRUE,
     main="xlimEqualLeftRight=TRUE")
plot(lik2, xTickLabelsPositive=FALSE, xlimEqualLeftRight=TRUE,
     main="xTickLabelsPositive=FALSE, xlimEqualLeftRight=TRUE")


## likert(tmp3)
likert(tmp3, layout=c(1,4))
likert(tmp3, layout=c(2,2), resize.height=c(2,1), resize.width=c(3,4))

likert(tmp4, layout=c(3,2))
likert(tmp4, layout=c(3,2),
       rightAxis=TRUE, between=list(x=3))
likert(tmp4, layout=c(3,2),
       auto.key=list(border=TRUE, padding.text=4, height=.5))
likert(tmp4, layout=c(3,2), strip.left=FALSE)
likert(tmp4, layout=c(3,2), strip.left=FALSE, horizontal=FALSE)
likert(tmp4, layout=c(3,2), strip.left=FALSE,
       auto.key=list(border=TRUE, padding.text=4, height=.5),
       resize.height=1:2)


## plot.likert interprets vectors as single-row matrices.
## http://survey.cvent.com/blog/customer-insights-2/box-scores-are-not-just-for-baseball
Responses <- c(15, 13, 12, 25, 35)
names(Responses) <- c("Strongly Disagree", "Disagree", "No Opinion",
                      "Agree", "Strongly Agree")
likert(Responses, main="Retail-R-Us offers the best everyday prices.",
       sub="This plot looks better in a 9in x 2.6in window.")

likert(Responses, horizontal=FALSE,
       aspect=1.5,
       main="Retail-R-Us offers the best everyday prices.",
       sub="This plot looks better in a 7in x 3in window.")
##
## reverse=TRUE  is needed for a single-column key with
## horizontal=FALSE and with space="right"
likert(Responses, horizontal=FALSE,
       aspect=1.5,
       main="Retail-R-Us offers the best everyday prices.",
       auto.key=list(space="right", columns=1,
                     reverse=TRUE, padding.text=2),
       sub="This plot looks better in a 4in x 3in window.")

## plot.likert will plot a single-column matrix as if it were entirely
## "No Opinion".
likert(as.matrix(Responses),
       main="This is silly, but it doesn't give an error")


likert(ProfChal[[2]],
       horizontal=FALSE,
       scales=list(x=list(rot=90)),
       auto.key=list(columns=1, space="right", reverse=TRUE),
       main="Vertical diverging stacked barcharts are possible.\nThey usually do not look good when the bar labels are long.",
       sub="This plot looks less bad in a 8.5in x 7in window.")

likert(ProfChal,
       horizontal=FALSE,
       scales=list(x=list(rot=90)),
       auto.key=list(columns=1, space="right", reverse=TRUE),
       main="Vertical diverging stacked barcharts are possible.\nThey usually do not look good when the bar labels are long.",
       sub="This plot looks less bad in a 8.5in x 7in window.")


## Diverging stacked bar charts have many uses in addition to plotting
## rating scales.  The AudiencePercent example, provided by Naomi
## B. Robbins, illustrates the age distribution of users of different
## brands of a product.  The original source for this example used
## multiple pie charts.

data(AudiencePercent)
AudiencePercent

likert(AudiencePercent,
      auto.key=list(between=1, between.columns=2),
      xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
      main="Target Audience",
      sub="This plot looks better in a 7in x 3.5in window.")

## Color palettes from RColorBrewer can be named.
##
## Since age is always positive and increases in a single direction,
## this example uses colors from a sequential palette for the age
## groups.  In this example we do not use a diverging palette that is
## appropriate when groups are defined by a characteristic, such as
## strength of agreement or disagreement, that can increase in two directions.

## Initially we use the "Blues" palette in the BrewerPaletteName argument.
likert(AudiencePercent,
       auto.key=list(between=1, between.columns=2),
       xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
       main="Target Audience",
       BrewerPaletteName="Blues",
       sub="This plot looks better in a 7in x 3.5in window.")


## The really light colors in the "Not run" example are too light.
## Therefore we use the col argument directly.  We chose to use an
## intermediate set of Blue colors selected from the "Blues" palette.
likert(AudiencePercent,
       auto.key=list(between=1, between.columns=2),
       xlab="Percentage of audience younger than 35 (left of zero) and older than 35 (right of zero)",
       main="Target Audience",
       col=brewer.pal(9, "Blues")[4:7],
       sub="This plot looks better in a 7in x 3.5in window.")

## Population Pyramid
data(USAge.table)
USA79 <- USAge.table[75:1, 2:1, "1979"]/1000000
PL <- likert(USA79,
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
as.pyramidLikert(PL)

likert(USAge.table[75:1, 2:1, c("1939","1959","1979")]/1000000,
       main="Population of United States 1939,1959,1979 (ages 0-74)",
       sub="Look for the Baby Boom",
       xlab="Count in Millions",
       ylab="Age",
       scales=list(
         y=list(
           limits=c(0,77),
           at=seq(1,76,5),
           labels=seq(0,75,5),
           tck=.5)),
       strip.left=FALSE, strip=TRUE,
       layout=c(3,1), between=list(x=.5))


Pop <- rbind(a=c(3,2,4,9), b=c(6,10,12,10))
dimnames(Pop)[[2]] <- c("Very Low", "Low", "High", "Very High")
likert(as.listOfNamedMatrices(Pop),
            as.percent=TRUE,
            resize.height="rowSums",
            strip=FALSE,
            strip.left=FALSE,
            main="Area and Height are proportional to 'Row Count Totals'.\nWidth is exactly 100%.")


## Professional Challenges example.
##
## The data for this example is a list of related likert scales, with
## each item in the list consisting of differently named rows.  The data
## is from a questionnaire analyzed in a recent Amstat News article.
## The study population was partitioned in several ways.  Data from one
## of the partitions (Employment sector) was used in the first example
## in this help file.  The examples here show various options for
## displaying all partitions on the same plot.
##
data(ProfChal)
## ProfChal
print(ProfChal, minlength=6)

## 1. Plot counts with rows in each panel sorted by positive counts.
##
likert(ProfChal,
       positive.order=TRUE,
       main="This works, but needs more specified arguments to look good",
       sub="This looks better in a 10inx7in window")
##
## Strip labels on left (the default for plot.likert).
## positive.order=TRUE is the recommended (but not default) setting for
## lists because the rownames within each item of the list are usually
## different.
##
ProfChalCountsPlot <-
likert(ProfChal, box.width=unit(.4,"cm"),
       strip.left.par=list(cex=.7, lines=5),
       main=list("Is your job professionally challenging?",
                 x=unit(.65, "npc")),
       xlab="Counts",
       positive.order=TRUE,
       rightAxis=TRUE,  ## display Row Count Totals
       sub="This plot looks better in a 10in x 7in window.")
ProfChalCountsPlot


## Strip labels on top (the default for most lattice plots).
## Change line spacing in the "Attitude" panel
names(ProfChal)[6] <- "Attitude toward Professional Recognition"
likert(ProfChal, box.width=unit(.3,"cm"),
    strip.left=FALSE, strip=TRUE,
    main=list("Is your job professionally challenging?", x=unit(.65, "npc")),
    xlab="Counts",
    positive.order=TRUE)
## Restore original line spacing
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## vertical bars
names(ProfChal)[6] <- "Prof Recog"
likert(ProfChal, horizontal=FALSE,
       positive.order=TRUE,
       scales=list(x=list(rot=45)),
       strip.par=list(cex=.7))
## Restore original line spacing
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"

## Eight sequences and orientations
names(ProfChal)[6] <- "Prof Recog"
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  reverse=FALSE, main="-h- default") ## default
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  reverse=TRUE,  main="-hr")
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, reverse=FALSE, main="---",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, reverse=TRUE,  main="--r",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  reverse=FALSE, main="ph-")
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  reverse=TRUE,  main="phr")
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, reverse=FALSE, main="p--",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, reverse=TRUE,  main="p-r",
       scales=list(x=list(rot=90)))

## Four sequences with the default value reverse=FALSE
likert(ProfChal, positive.order=FALSE, horizontal=TRUE,  main="-h default") ## default
likert(ProfChal, positive.order=FALSE, horizontal=FALSE, main="--",
       scales=list(x=list(rot=90)))
likert(ProfChal, positive.order=TRUE,  horizontal=TRUE,  main="ph")
likert(ProfChal, positive.order=TRUE,  horizontal=FALSE, main="p-",
       scales=list(x=list(rot=90)))

names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"


## 2. Plot percents with rows in each panel sorted by positive percents.
##    This is a different sequence than the counts.  Row Count Totals are
##    displayed on the right axis.
ProfChalPctPlot <-
likert(ProfChal,
    as.percent=TRUE,    ## implies display Row Count Totals
    box.width=unit(.4,"cm"),
    strip.left.par=list(cex=.7, lines=5),
    main=list("Is your job professionally challenging?",
              x=unit(.65, "npc")),
    xlab="Percent",
    positive.order=TRUE,
    sub="This plot looks better in a 10in x 7in window.")
ProfChalPctPlot


## 3. Putting both percents and counts on the same plot, both in
##    original sort order.

## Change name in the "Attitude" panel
names(ProfChal)[6] <- "Prof Recog"

LikertPercentCountColumns(ProfChal,
                          main="Is your job professionally challenging?",
                          sub="LikertPercentCountColumns test, 9x8 window")

## Restore original name
names(ProfChal)[6] <- "Attitude\ntoward\nProfessional\nRecognition"


## The ProfChal data is done again with explicit use of ResizeEtc
## in ?HH:::ResizeEtc
