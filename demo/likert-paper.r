## This file requires the HH package, version 2.3-25 or newer.
## Many of the examples will require stretching of the display window.
## Recommended width by height values are included in the comments.

library(HH)

data(ProfChal)
ProfChal.df <- as.likertDataFrame(ProfChal)

PCQ <- as.character(ProfChal.df$Question)
PCQ[6] <- "Other (including retired, students,\nnot employed, etc.)" ## insert line break
ProfChal.df$Question <- factor(PCQ, levels=PCQ)
attributes(ProfChal.df)$names.dimnames <- c("Characteristic","ResponseLevel")

## Figure 1
ProfChalPctPlot <- ## 8in x 7in  ## ProfChal.pdf
likert(Question ~ . | Subtable, ProfChal.df,
       as.percent=TRUE,    ## implies display Row Count Totals
       box.width=unit(.4,"cm"),
       strip.left.par=list(cex=.6, lines=5),
       main=list("Is your job professionally challenging?", x=unit(.65, "npc")),
       positive.order=TRUE,
       par.settings=list(layout.widths=list(ylab.right=7)))
ProfChalPctPlot

## Table 1
EmpRows <- ProfChal.df$Subtable == "Employment sector"
ProfChal2 <- ProfChal.df[EmpRows, 1:5]
rownames(ProfChal2) <- substr(ProfChal.df[EmpRows, "Question"], 1, 5)
ProfChal2
as.likert(ProfChal2)


## Table 2
rownames(ProfChal2) <- ProfChal.df$Question[EmpRows]
ProfChal2[1:5]

## Figure 2
likert(Question ~ . , data=ProfChal.df[EmpRows,],  ## 8in x 3in  ## PC2C.pdf
       xlab="Count",
       main="Is your job professionally challenging?")

## Figure 3
likert(Question ~ . , data=ProfChal.df[EmpRows,],  ## 8in x 3in  ## PC2Cpct.pdf
       xlab="Count", as.percent=TRUE,
       main="Is your job professionally challenging?")

## Figure 4
likert(Question ~ . , data=ProfChal.df[EmpRows,],  ## 8in x 3in  ## PC2Cpctpo.pdf
       as.percent=TRUE,
       main="Is your job professionally challenging?",
       positive.order=TRUE)


## Figure 5
## ProfChal6_131.pdf
## 9in x 9in
## ProfChal6_132.pdf
## 9in x 9in
data(ProfChal)
ProfChal.df <- as.likertDataFrame(ProfChal)
##
AA <- likert(Question ~ . , ProfChal.df[1,],
             main=levels(ProfChal.df$Subtable)[1],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
BB <- likert(Question ~ . , ProfChal.df[2:6,],
             main=levels(ProfChal.df$Subtable)[2],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
CC <- likert(Question ~ . , ProfChal.df[7:10,],
             main=levels(ProfChal.df$Subtable)[3],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
DD <- likert(Question ~ . , ProfChal.df[11:12,],
             main=levels(ProfChal.df$Subtable)[4],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
EE <- likert(Question ~ . , ProfChal.df[13:14,],
             main=levels(ProfChal.df$Subtable)[5],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
FF <- likert(Question ~ . , ProfChal.df[15:16,],
             main=levels(ProfChal.df$Subtable)[6],
             as.percent=TRUE, box.width=unit(.4,"cm"),
             positive.order=TRUE)
##
print(AA, more=TRUE,  split=c(1,1,1,3))
print(BB, more=TRUE,  split=c(1,2,1,3))
print(CC, more=FALSE,  split=c(1,3,1,3))
## ProfChal6_131.pdf
print(DD, more=TRUE,  split=c(1,1,1,3))
print(EE, more=TRUE,  split=c(1,2,1,3))
print(FF, more=FALSE, split=c(1,3,1,3))
## ProfChal6_132.pdf


## Figure 1, reprise
tmp <- resizePanels(c(AA,BB,CC,DD,EE,FF, layout=c(1,6), x.same=TRUE), c(1,5,4,2,2,2)-.5)
tmp <- update(tmp, main=ProfChalPctPlot$main, strip.left=TRUE, par.strip.text=list(cex=.6, lines=5))
tmp$condlevels <- list(ListNames=levels(ProfChal.df$Subtable))
tmp$y.limits <- lapply(list(AA,BB,CC,DD,EE,FF), `[[`, "y.limits")
tmp$par.settings$layout.widths$ylab.right <- 7
tmp

## Figure 6
## http://www.morst.govt.nz/Documents/publications/researchreports/
## Staying-in-Science-summary.pdf

## New Zealand students still taking science in Year 13.
## "Students' interest in science, and views about secondary science teaching"
## "Students' feelings about making tertiary study decisions"
data(NZScienceTeaching)
NZScienceTeaching.df <- as.likertDataFrame(NZScienceTeaching)
attributes(NZScienceTeaching.df)$names.dimnames <- c("Question", "Agreement")

likert(Question ~ . | Subtable, NZScienceTeaching.df, ## 10in x 5in  ## NZscienceteaching.pdf
       main="New Zealand Students Still Taking Science in Year 13",
       auto.key=list(between=.8, cex=.8),
       xlab="Percent",
       scales=list(y=list(cex=.9)),
       strip.left.par=list(cex=1.2, lines=1.3))


## Figure 7
## 9in x 7.5in   ## SFF8121.pdf
data(SFF8121)
SFF8121.df <- as.likertDataFrame(SFF8121)
## as.MatrixList(SFF8121)
likert(Question ~ . | Subtable, SFF8121.df,
       layout=c(2,1), between=list(x=1),
       scales=list(x=list(alternating=FALSE)),
       strip.left=FALSE,
       xlab="Percent",
       main="Student Feedback Forms, Spring 2010")


## Figure 8
## PopUSA1939-1979.pdf
## this needs a 17x8 window
data(USAge.table) ## from package:latticeExtra
## tmp <- USAge.table[75:1, 2:1, seq(40,80,10)]
tmp.df <- as.likertDataFrame(USAge.table[75:1, 2:1, seq(40,80,10)]/1000000)
names(tmp.df)[3] <- "Age"
names(tmp.df)[4] <- "Year"
PL5 <-
likert(Age ~ . | Year, tmp.df,
       strip.left=FALSE, strip=TRUE,
       main="Population of United States (ages 0-74)",
       xlab="Count in Millions",
       ylab="Age",
       sub="Look for the Baby Boom",
       scales=list(
         y=list(
           limits=c(0,77),
           at=seq(1,76,5),
           labels=seq(0,75,5),
           alternating=3,
           tck=1),
         x=list(alternating=FALSE, at=-2:2)),
       auto.key=list(title=NULL),
       layout=c(5,1), between=list(x=.5))


## 7x8 window
PL <- likert(Age ~ . , tmp.df[tmp.df$Year=="1979",],
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
## Figure 9
## popUSA1979.pdf
as.pyramidLikert(PL)


## Figure 10
## top is historical
## bottom
## ProfitDividend.pdf
## 4.5in x 2.75in
data(ProfDiv)
ProfDiv.df <- as.likertDataFrame(ProfDiv)
names(ProfDiv.df)[5] <- "Year"
likert(Year ~ ., ProfDiv.df, horizontal=FALSE, positive.order=FALSE,
       auto.key=list(reverse=TRUE, columns=1, space="right",
         size=4, padding.text=1.5),
       ylab="Year", xlab="Per Cent",
       main=paste("Profit-and-Dividend Status of 348 Corportations",
         "in the United States\nfor the period from 1929 to 1935."),
       sub="Dun's Review, April 1938",
       par.settings=list(layout.widths=list(right.padding=2.5)))


## Figure 11
## PoorChildren
## Colors taken from NY Times figure using Snagit editor
PCWPpalette <- c("#A8C2C0", "#C9E9E6", "#D0E39A", "#A3B37B")
data(PoorChildren)
PoorChildren.df <- as.likertDataFrame(PoorChildren)
names(PoorChildren.df)[5] <- "PctPoorHH"
PoorChildren.df$Subtable <- PoorChildren.df$PctPoorHH
likert(PctPoorHH ~ . | Subtable, PoorChildren.df,
       col=PCWPpalette, as.percent=TRUE,
       ylab="Percent of poor households in area",
       xlab="Percent of Children",
       xlab.top=c("No Working Parents", "One or more Working Parents"),
       ylab.right="Row Count Totals",
       main="Poor Children, Working Parents",
       strip=FALSE,
       strip.left=FALSE,
       rightAxisLabels=format(rowSums(PoorChildren), big.mark=","),
       resize.height="rowSums",
       par.settings=list(
         axis.line=list(col="transparent"),
         layout.widths=list(ylab.right=7)))

## Figure 12   ## not yet in formula method
tmp4 <- colSums(PoorChildren)[c(2,1,3,4)]
ByWP <- rbind(matrix(0,4,2,dimnames=list(letters[1:4],NULL)),
              cbind(NWP=tmp4, '1+WP'=tmp4))
ByWP[cbind(5:8, c(2,2,1,1))] <- 0
ByWP
##
PL4vert <-
likert(as.listOfNamedMatrices(t(ByWP)), as.percent=TRUE, horizontal=FALSE,
       col=c(rep("transparent",4), PCWPpalette[c(2,1,3,4)]),
       strip.left=FALSE, strip=FALSE,
       ylab.right=list(c("Moderately\nPoor", "Extremely\nPoor"), rot=0),
       rightAxisLabels=format(colSums(ByWP), big.mark=","),
       resize.width=1,
       resize.height=colSums(ByWP), ## horizontal=FALSE applies this to width
       layout=c(2,1),
       box.ratio=1000,
       xlab="Percent of Children, All Areas",
       xlab.top=c("No Working Parents", "One or more Working Parents"),
       ylab="Number of Children",
       main="Poor Children, Working Parents",
       par.settings=list(axis.line=list(col="transparent"),
         layout.widths=list(axis.right=0, ylab.right=1)))
## PL4vert
PL4vert$legend$bottom$args$text <-
            PL4vert$legend$bottom$args$text[c(6,5,7,8)]
PL4vert$legend$bottom$args$rect$col <-
            PL4vert$legend$bottom$args$rect$col[c(6,5,7,8)]
PL4vert$legend$bottom$args$columns <- 4
for (i in seq(along=PL4vert$x.limits))
  PL4vert$x.limits[[i]] <- names(PL4vert$x.limits[[i]])
PL4vert

{
## Figure 12 revised 1
  tmp2 <- data.frame(t(apply(PoorChildren.df[1:4], 2, sum)), all=1, check.names=FALSE)
  likNWP <- likert(all ~ ., data=tmp2[,c(2:1,5)], horizontal=FALSE, Ref=0,
                   col=PCWPpalette[2:1], as.percent=TRUE,
                   xlab.top="No Working Parents",
                   xlab="Percent of Children, All Areas",
                   ylab.right=list(c("Moderately\nPoor", "Extremely\nPoor"), rot=0),
                   ylab=format(sum(tmp2[,2:1]), big.mark=","))
  likWP  <- likert(all ~ ., data=tmp2[,c(3:4,5)], horizontal=FALSE, Ref=0,
                   col=PCWPpalette[3:4], as.percent=TRUE,
                   xlab.top="One or More Working Parents",
                   xlab="Percent of Children, All Areas",
                   ylab.right=list(c("Moderately\nPoor", "Extremely\nPoor"), rot=0),
                   ylab=format(sum(tmp2[,3:4]), big.mark=","))
  lik <- resizePanels(c(likNWP, likWP), w=c(sum(tmp2[,2:1]), sum(tmp2[,3:4])))
  lik <- update(lik, par.settings=list(axis.line=list(col="transparent"),
                       layout.widths=list(axis.right=0, ylab.right=1.2)))
  lik <- update(lik,
                between=list(x=0),
                scales=list(x=list(limits=list(
                              format(sum(tmp2[,2:1]), big.mark=","),
                              format(sum(tmp2[,3:4]), big.mark=",")))),
                xlab="Number of Children",
                xlab.top=c("No Working Parents", "One or More Working Parents"),
                main="Poor Children, Working Parents",
                box.ratio=1000)
  lik$legend$bottom$args$text <- c(likNWP$legend$bottom$args$text[2:1],
                                   likWP$legend$bottom$args$text)
  lik$legend$bottom$args$rect$col <- c(likNWP$legend$bottom$args$rect$col[2:1],
                                       likWP$legend$bottom$args$rect$col)
  lik$legend$bottom$args$columns <- 4
  lik
}

{
## Figure 12 revised 2
  PCWP <- data.frame(t(apply(PoorChildren.df[c(2,1,3,4)], 2, sum)), check.names=FALSE)
  PCWP2 <- cbind(rbind(PCWP, PCWP),
                 WP=ordered(c("NWP","1+WP"), levels=c("NWP","1+WP")),
                 all=1)
  PCWP2[1, 3:4] <- 0
  PCWP2[2, 1:2] <- 0

  PClik <-
    likert(all ~ . | WP, data=PCWP2, horizontal=FALSE, Ref=0,
           col=PCWPpalette[c(2,1,3,4)], as.percent=TRUE)

  PClik <- resizePanels(PClik, w=c(sum(PCWP2[,1:2]), sum(PCWP2[,3:4])))
  PClik <- update(PClik, par.settings=list(
                           axis.line=list(col="transparent"),
                           layout.widths=list(axis.right=0, ylab.right=1.2)))
  PClik <- update(PClik,
                  scales=list(x=list(limits=list(
                                       format(sum(PCWP2[,1:2]), big.mark=","),
                                       format(sum(PCWP2[,3:4]), big.mark=",")))),
                  xlab="Number of Children",
                  xlab.top=c("No Working Parents", "One or More Working Parents"),
                  ylab="Percent of Children, All Areas",
                  ylab.right=list(c("Moderately\nPoor", "Extremely\nPoor"), rot=0),
                  main="Poor Children, Working Parents",
                  box.ratio=1000,
                  strip=FALSE)
  PClik$legend$bottom$args$text <- PClik$legend$bottom$args$text[c(2,1,3,4)]
  PClik$legend$bottom$args$rect$col <-  PClik$legend$bottom$args$rect$col[c(2,1,3,4)]
  PClik
}

{
## Figure 12 revised 2a
  PCWP <- data.frame(t(apply(PoorChildren.df[c(2,1,3,4)], 2, sum)), check.names=FALSE)
  PCWP2 <- cbind(rbind(PCWP, PCWP),
                 WP=ordered(c("NWP","1+WP"), levels=c("NWP","1+WP")),
                 all=1)
  PCWP2[1, 3:4] <- 0
  PCWP2[2, 1:2] <- 0
  PCWPcounts <- c(sum(PCWP2[,1:2]), sum(PCWP2[,3:4]))
  PClik <-
    likert(all ~ . | WP, data=PCWP2, horizontal=FALSE, Ref=0,
           col=PCWPpalette[c(2,1,3,4)], as.percent=TRUE,
           ## above is the working part.  below is cosmetics
           par.settings=list(
             axis.line=list(col="transparent"),
             layout.widths=list(axis.right=0, ylab.right=1.2)),
           xlab="Number of Children",
           xlab.top=c("No Working Parents", "One or More Working Parents"),
           ylab="Percent of Children, All Areas",
           ylab.right=list(c("Moderately\nPoor", "Extremely\nPoor"), rot=0),
           main="Poor Children, Working Parents",
           box.ratio=1000,
           strip=FALSE)
  PClik$x.limits <- as.list(format(PCWPcounts, big.mark=","))
  PClik$legend$bottom$args$text <- PClik$legend$bottom$args$text[c(2,1,3,4)]
  PClik$legend$bottom$args$rect$col <-  PClik$legend$bottom$args$rect$col[c(2,1,3,4)]
  PClik <- resizePanels(PClik, w=PCWPcounts)
  PClik
}


{
  ## Figure 12 revised 3
  ## This variant doesn't allow width control.
  ## box.width=rep(rowSums(PCWP2[,1:4]), each=2)/8000000
  ## applies to each item in the stacked bar, not to the entire bar
  ##
  ## PClik <-
  ##   likert(WP ~ ., data=PCWP2[1:5], horizontal=FALSE, Ref=0,
  ##          col=PCWPpalette[c(2,1,3,4)], as.percent=TRUE)
}

## Figure 13
## AudiencePercent.pdf
## 7in x 4in
data(AudiencePercent)
AudiencePercent.df <- as.likertDataFrame(AudiencePercent)
AudiencePercent.df$Subtable <- factor("A")
names(AudiencePercent.df)[5] <- "Brands"
likert(Brands ~ . , AudiencePercent.df,
       positive.order=TRUE,
       auto.key=list(between=1, between.columns=2),
       xlab=paste("Percentage of audience younger than 35",
         "(left of zero) and older than 35 (right of zero)"),
       ylab="Brands",
       main="Brand A has the most even distribution of ages",
       scales=list(x=list(at=seq(-90,60,10),
                     labels=as.vector(rbind("", abs(seq(-80,60,20)))))),
       col=brewer.pal(9, "Blues")[4:7])


## Grouped Bar Chart  ## we are not recommending this plot for this data
## Figure 14
tmp <- data.frame(ProfChal[[2]])
dimnames(tmp)[[1]][5] <- "Other (including retired, students,\nnot employed, etc.)"
tmp <- data.frame(stack(tmp),
                  Employment=row.names(tmp))
names(tmp)[1:2] <- c("Count", "Agreement")
tmp$Agreement <- factor(tmp$Agreement, unique(tmp$Agreement))
tmp$Percent <- as.vector(ProfChal[[2]] / rowSums(ProfChal[[2]]))
## Figure 14a
## PctAEv.pdf ## 16in x 6in
barchart(Percent ~ Agreement | Employment, data=tmp,
         col=brewer.pal(5, "RdBu"),
         horizontal=FALSE, layout=c(5,1), between=list(x=1), box.ratio=10,
         origin=0,
         par.strip.text=list(cex=.9, lines=2.5),
         scales=list(x=list(rot=90)),
         main="Percent ~ Agreement | Employment")
##
## Figure 14b
## PctAEh.pdf ## 16in x 3in
barchart(Agreement ~ Percent | Employment, data=tmp,
         col=brewer.pal(5, "RdBu"),
         horizontal=TRUE, layout=c(5,1), between=list(x=1), box.ratio=10,
         origin=0,
         par.strip.text=list(cex=.9, lines=2.5),
         scales=list(x=list(alternating=1)),
         main="Agreement ~ Percent | Employment")



## Heat Map   ## we are not recommending this plot for this data
## Figure 15 ## 8in x 5in
## PC2CpctpoHM.pdf
likert(Question ~ . , data=ProfChal.df[EmpRows,], ## 8in x 3in  ## PC2CpctpoHM.pdf
       as.percent=TRUE,
       main="Is your job professionally challenging?",
       ReferenceZero=0,
       col=brewer.pal(5, "RdBu"),  ## col= needed because Ref=0 sets all colors to positive
       box.ratio=20)


## Figure 16
## screen shot from Tableau worksheet JSS_figures6-1.twbx
## TableauWorksheet.pdf

## Figure 17
## pdf file drawn by Tableau
## Figure12b.pdf
