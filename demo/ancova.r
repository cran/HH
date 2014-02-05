## demo/ancova.r

library(HH)
data(hotdog)

script_HH <- function(scriptname) {
  HH.index <- match("package:HH", search(), 0)
  paste(searchpaths()[HH.index], "scripts", scriptname, sep="/")
}

cat("This example is based on the last graph produced by the script\n", script_HH("Ch10-regbb.r"), "\n")



removeLegendAxes <-
       function(x) {
         x$ylab <- NULL
         x$xlab <- NULL
         x$legend <- NULL
         x$x.scales$alternating <- 0
         x$y.scales$alternating <- 0
         x$x.scales$tck <- c(0,0)
         x$y.scales$tck <- c(0,0)
         x$par.strip.text$cex <- .7
         x
      }

## regression
## same line: common intercept and common slope
     hC.aov <- ancova(Sodium ~ Calories, groups=Type, data=hotdog,
                      par.strip.text=list(cex=1.2), ylim=c(140,700))

## horizontal lines: zero slope and separate intercepts
  hotdog.aov <- ancova(Sodium ~ Type, data=hotdog, x=Calories,
                       par.strip.text=list(cex=1.2), ylim=c(140,700))

## analysis of covariance
## analysis with a concomitant explanatory variable
## parallel lines: separate intercepts and common slope
hCT.aov <- ancova(Sodium ~ Calories + Type, data=hotdog,
                  par.strip.text=list(cex=1.2), ylim=c(140,700))

## interaction: separate intercepts and slopes
hCTi.aov <- ancova(Sodium ~ Calories * Type, data=hotdog,
                   par.strip.text=list(cex=1.2), ylim=c(140,700))


## 2 x 3, with empty spots
print(position=c(.03, .31,  .53, .62), more=TRUE,
      removeLegendAxes(attr(  hC.aov,     "trellis"))
      )
print(position=c(.50, .00, 1.00, .31), more=TRUE,
      removeLegendAxes(attr(  hotdog.aov, "trellis"))
      )
print(position=c(.50, .31, 1.00, .62), more=TRUE,
      removeLegendAxes(attr(  hCT.aov,    "trellis"))
      )
print(position=c(.50, .62, 1.00, .93), more=TRUE,
      removeLegendAxes(attr(  hCTi.aov,   "trellis"))
      )

## column labeling
print(position=c(.17, 0, .42, .10), more=TRUE,
      xyplot(0 ~ .5, panel=function(...){}, xlab=expression("constant intercept" ~~ alpha),
             ylab="", scales=list(draw=FALSE),
             par.settings = list(axis.line = list(col = "transparent")))
)
print(position=c(5/8, 0, 7/8, .10), more=TRUE,
      xyplot(0 ~ .5, panel=function(...){}, xlab=expression("variable intercept" ~~ alpha),
             ylab="", scales=list(draw=FALSE),
             par.settings = list(axis.line = list(col = "transparent")))
)

## row labeling
print(position=c(0, .09, .10, .19), more=TRUE,
      xyplot(0 ~ .5, panel=function(...){}, ylab=expression("zero slope" ~~ beta==0),
             xlab="", scales=list(draw=FALSE),
             par.settings = list(axis.line = list(col = "transparent")))
)
print(position=c(0, .41, .10, .51), more=TRUE,
      xyplot(0 ~ .5, panel=function(...){}, ylab=expression("constant slope" ~~ beta),
             xlab="", scales=list(draw=FALSE),
             par.settings = list(axis.line = list(col = "transparent")))
)
print(position=c(0, .76, .10, .86), more=TRUE,
      xyplot(0 ~ .5, panel=function(...){}, ylab=expression("variable slope" ~~ beta),
             xlab="", scales=list(draw=FALSE),
             par.settings = list(axis.line = list(col = "transparent")))
)

## main title
print(position=c(0, .90, 1, 1), more=FALSE,
      xyplot(0 ~ 0, panel=function(...){},
             xlab=NULL, ylab=NULL,
             scales=list(draw=FALSE),
             main="Composite graph illustrating four models with a factor and a covariate",
             par.settings = list(axis.line = list(col = "transparent"))))
