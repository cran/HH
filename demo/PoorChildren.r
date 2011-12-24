## Poor Children, Working Parents

## source("PoorChildren.r", echo=TRUE)
## R code by Richard Heiberger, December 20, 2011
##
## This file requires the HH package, version 2.2-22 or newer.

require(HH)
if (packageVersion("HH") < "2.2-22") stop("Please update HH to version 2.2-22 or newer.")

try(windows.options(record=TRUE)) ## windows.
## The try() will allow this to go through if a different device is in use.

## Colors taken from NY times figure using Snagit editor
PCWPpalette <- c("#A8C2C0", "#C9E9E6", "#D0E39A", "#A3B37B")

## for the Forbes online article we needed 640x480 pixels
## http://www.forbes.com/sites/naomirobbins/2011/12/20/alternative-to-charles-blows-figure-in-newts-war-on-poor-children-2/
## try(windows(width=6.6666666666666661, height=5.000)) ## this gives PNG with 640 x 480 pixels
## The try() will allow this to go through if a different device is in use.
## The default is
## windows(width=7, height=7) ## this gives PNG with 672 x 772 pixels

data(PoorChildren)

## > PoorChildren
##            Extremely.poor.NWP Moderately.poor.NWP Moderately.poor.1+WP Extremely.poor.1+WP
## 10 or less             606352              354007               930909              337828
## 10 to 15              1001894              641736              1548697              566705
## 15 to 20               998587              629566              1417142              535242
## 20 to 25               533108              308569               637017              251701
## 25 to 30               406372              242874               457117              197478
## 30 or more             391888              197773               348115              183158
## >


## Figure 2a
PCcount <- plot.likert(PoorChildren, col=PCWPpalette,
                       ylab="Percent of poor households in area",
                       xlab="Number of Children",
                       xlab.top=c("No Working Parents", "One or more Working Parents"),
                       ylab.right="Row Count Totals",
                       main="Poor Children, Working Parents",
                       scales=list(
                         x=list(
                           at=seq(-2,2,1)*1000000,
                           labels=c("2,000,000", "1,000,000","0","1,000,000","2,000,000"))),
                       rightAxisLabels=format(rev(rowSums(PoorChildren)), big.mark=","))
## PCcount
PCcount2 <- update(PCcount, par.settings=list(axis.line=list(col="transparent")))
PCcount2
## update(PCcount2, sub="Figure 2a")



## Figure 2b
PCpercent <- plot.likert(PoorChildren, col=PCWPpalette, as.percent=TRUE,
                         ylab="Percent of poor households in area",
                         xlab="Percent of Children",
                         xlab.top=c("No Working Parents", "One or more Working Parents"),
                         ylab.right="Row Count Totals",
                         main="Poor Children, Working Parents",
                         scales=list(
                           x=list(
                             at=seq(-40,60,20),
                             labels=c(40,20,0,20,40,60))),
                         rightAxisLabels=format(rev(rowSums(PoorChildren)), big.mark=","))
## PCpercent
PCpercent2 <- update(PCpercent, par.settings=list(axis.line=list(col="transparent")))
PCpercent2
## update(PCpercent2, sub="Figure 2b")


## Figure 2
PL3 <-
plot.likert(as.listOfNamedMatrices(PoorChildren),
            col=PCWPpalette, as.percent=TRUE,
            ylab="Percent of poor households in area",
            xlab="Percent of Children",
            xlab.top=c("No Working Parents", "One or more Working Parents"),
            ylab.right="\n\n\n\nRow Count Totals", ##"Row Count Totals",
            main="Poor Children, Working Parents",
            ## box.width=10*unit(rowSums(abs(x))/1000000, "mm"), ## ignored
            scales=list(
              x=list(
                at=seq(-40,60,20),
                labels=c(40,20,0,20,40,60))),
            strip=FALSE,
            strip.left=FALSE,
            box.ratio=1000,
            rightAxisLabels=format(rowSums(PoorChildren), big.mark=","))
## PL3$par.settings$layout.widths$right.padding <- 0  ## these 0 paddings are to make it fit in a 640x480 window
## PL3$par.settings$layout.widths$left.padding <- 0
## PL3$par.settings$layout.heights$top.padding <- 0
## PL3$par.settings$layout.heights$bottom.padding <- 0
PL3 <- update(PL3, par.settings=list(axis.line=list(col="transparent")))
PL3R <- ResizeEtc(PL3, resize.height=rev(rowSums(abs(PoorChildren))/1000000))
PL3R
## update(PL3R, sub="Figure 2")


## Figure 3
tmp5 <- as.likert(colSums(PoorChildren))
PCpercentAvg <- plot.likert(tmp5, col=PCWPpalette, as.percent=TRUE,
                            xlab="Percent of Children",
                            ylab=list("All Areas", rot=0),
                            xlab.top=c("No Working Parents", "One or more Working Parents"),
                            ylab.right="Total All Children",
                            main="Poor Children, Working Parents",
                            box.ratio=1000,
                            scales=list(
                              x=list(
                                at=seq(-40,60,20),
                                labels=c(40,20,0,20,40,60))),
                            rightAxisLabels=format(sum(PoorChildren), big.mark=","))
PCpercentAvg2 <- update(PCpercentAvg, par.settings=list(axis.line=list(col="transparent")))
PCpercentAvg2
## update(PCpercentAvg2, sub="Figure 3")


## Figure 4
tmp6 <- abs(tmp5)
dim(tmp6) <- c(2,2)
dimnames(tmp6) <- list(c("Moderately Poor","Extremely Poor"), c("NWP","1+WP"))
tmp6 <- t(tmp6)
tmp6
PCpercentTotal <- plot.likert(tmp6, col=PCWPpalette, as.percent="noRightAxis",
                              xlab="Percent of Children, All Areas",
                              xlab.top=c("No Working Parents", "One or more Working Parents"),
                              ylab="Number of Children",
                              ylab.right=list(c("Moderately\nPoor","Extremely\nPoor"), rot=0),
                              main="Poor Children, Working Parents",
                              horizontal=FALSE)
## PCpercentTotal
PCpercentTotal$x.limits <- format(rowSums(tmp6), big.mark=",")
PCT2 <- update(PCpercentTotal, par.settings=list(axis.line=list(col="transparent")))
##
PCT2NWP <- PCT2
PCT2YWP <- PCT2
PCT2YZZ <- PCT2
##
PCT2NWP$panel.args[[1]]$y[c(2,4)] <- 0
PCT2YWP$panel.args[[1]]$y[c(1,3)] <- 0
PCT2YZZ$panel.args[[1]]$y[] <- 0
PCT2NWP$panel.args.common$box.width <- 7.5*unit(rowSums(tmp6)[[1]]/1000000, "mm")
PCT2YWP$panel.args.common$box.width <- 7.5*unit(rowSums(tmp6)[[2]]/1000000, "mm")
PCT2NWP$panel.args.common$col <- c("#C9E9E6", "#A8C2C0")
PCT2YWP$panel.args.common$col <- c("#D0E39A", "#A3B37B")
PCT2NWP$panel.args.common$border <- c("#C9E9E6", "#A8C2C0")
PCT2YWP$panel.args.common$border <- c("#D0E39A", "#A3B37B")
print(PCT2NWP, more=TRUE)
print(PCT2YWP, more=TRUE)
print(PCT2YZZ, more=FALSE)
## print(update(PCT2NWP, sub="Figure 4"), more=TRUE)
## print(update(PCT2YWP, sub="Figure 4"), more=FALSE)
## print(update(PCT2YZZ, sub="Figure 4"), more=FALSE)



## Figure 5
tmp9 <- abs(tmp5)
dim(tmp9) <- c(2,2)
dimnames(tmp9) <- list(c("Moderately Poor","Extremely Poor"), c("NWP","1+WP"))
tmp9
PCpercentTotalB <- plot.likert(tmp9, col=PCWPpalette, as.percent="noRightAxis",
                               xlab="Percent of Children, All Areas",
                               xlab.top=c("Moderately Poor", "Extremely Poor"),
                               ylab.right=list(c("No\nWorking\nParents","One or more\nWorking\nParents"), rot=0),
                               ylab="Number of Children",
                               main="Poor Children, Working Parents",
                               horizontal=FALSE)
## PCpercentTotalB
PCpercentTotalB$x.limits <- format(rowSums(tmp9), big.mark=",")
PCTB2 <- update(PCpercentTotalB, par.settings=list(axis.line=list(col="transparent")))
##
PCTB2MP <- PCTB2
PCTB2EP <- PCTB2
PCTB2ZZ <- PCTB2
##
PCTB2MP$panel.args[[1]]$y[c(2,4)] <- 0
PCTB2EP$panel.args[[1]]$y[c(1,3)] <- 0
PCTB2ZZ$panel.args[[1]]$y[] <- 0
PCTB2MP$panel.args.common$box.width <- 7.5*unit(rowSums(tmp9)[[1]]/1000000, "mm")
PCTB2EP$panel.args.common$box.width <- 7.5*unit(rowSums(tmp9)[[2]]/1000000, "mm")
PCTB2MP$panel.args.common$col <- c("#C9E9E6", "#D0E39A")
PCTB2EP$panel.args.common$col <- c("#A8C2C0", "#A3B37B")
PCTB2MP$panel.args.common$border <- c("#C9E9E6", "#D0E39A")
PCTB2EP$panel.args.common$border <- c("#A8C2C0", "#A3B37B")
print(PCTB2MP, more=TRUE)
print(PCTB2EP, more=TRUE)
print(PCTB2ZZ, more=FALSE)
## print(update(PCTB2MP, sub="Figure 5"), more=TRUE)
## print(update(PCTB2EP, sub="Figure 5"), more=FALSE)
## print(update(PCTB2ZZ, sub="Figure 5"), more=FALSE)
