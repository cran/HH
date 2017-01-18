## The geometry of regression coefficients.
## The change in predicted y when x1 is changed one unit holding all
## other x variables constant.


data(fat)

if (FALSE) { ## interactive plot not shown by default
  ## interactive plot based on Figure 9.1, HHscriptnames(9), chunk 2
  ## (slightly revised)
  fat2.resid <- resid(lm(bodyfat ~ biceps + abdomin, data=fat))
  ##
  car::scatter3d(bodyfat ~ biceps + abdomin, data=fat, fit="linear",
                 ## changed order of x variables
                 residuals="squares",
                 bg="white", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE,
                 square.color = "gray80", surface.col="#a6cafe",
                 surface.alpha=.3, sphere.size=.7,
                 point.col=c("red","green")[1+(fat2.resid >= 0)],
                 grid.lines=5) ## grid.lines argument specified
}

## This figure is based on static Figure 9.1 in the First Edition of HH.
## The First Edition code is in  HHscriptnames(9, ed=1)
## in the first section, labeled rega.f4.s

## regr2.plot is built on persp in base graphics.
## The returned value 'persp.out' is the transformation from 3d to 2d.
## See ?persp for details.
persp.out <-
regr2.plot(fat[,"abdomin"], xlab="abdomin",
           fat[,"biceps"],  ylab="biceps",
           fat[,"bodyfat"], zlab="bodyfat",
           resid.plot="square",
           theta=120, phi=20, r=sqrt(15),
           plot.back.planes=FALSE,
           main="Least-squares with two X-variables")


## These additional lines are constructed by modifying the first and
## second examples in ?persp .
## Incrementally add points, labels, and lines to existing persp plot
## using trans3d() :

fat.lm <- lm(bodyfat ~ abdomin + biceps, data=fat)
fat.new <- data.frame(abdomin=c(140, 120), biceps=30)
bodyfat.pred <- predict(fat.lm, newdata=fat.new)
fat.new <- cbind(fat.new, bodyfat=bodyfat.pred)
fat.new

fat.lines <- rbind(fat.new, fat.new, fat.new)
fat.lines
fat.lines[1:2, "bodyfat"] <- 0
fat.lines[3, "bodyfat"] <- fat.lines[4, "bodyfat"]
fat.lines

points(trans3d(x=fat.lines$abdomin,
               y=fat.lines$biceps,
               z=fat.lines$bodyfat,
               pmat=persp.out),
       pch=c("1","2","3","4","5","6"))

## grid line on base, change abdomin 20 units while holding biceps constant
lines(trans3d(x=fat.lines$abdomin[1:2],
              y=fat.lines$biceps[1:2],
              z=fat.lines$bodyfat[1:2],
              pmat=persp.out),
      col="black", lwd=3)

## Same grid line projected onto regression surface
lines(trans3d(x=fat.lines$abdomin[5:6],
              y=fat.lines$biceps[5:6],
              z=fat.lines$bodyfat[5:6],
              pmat=persp.out),
      col="black", lwd=3)

## vertical lines connecting base and regression surface
lines(trans3d(x=fat.lines$abdomin[c(1,5)],
              y=fat.lines$biceps[c(1,5)],
              z=fat.lines$bodyfat[c(1,5)],
              pmat=persp.out),
      col="blue", lwd=3)
lines(trans3d(x=fat.lines$abdomin[c(2,6)],
              y=fat.lines$biceps[c(2,6)],
              z=fat.lines$bodyfat[c(2,6)],
              pmat=persp.out),
      col="blue", lwd=3)

## base grid line projected onto surface parallel to base
## at z-height of abdomin=120 and biceps=30, bodyfat=39.69418
lines(trans3d(x=fat.lines$abdomin[3:4],
              y=fat.lines$biceps[3:4],
              z=fat.lines$bodyfat[3:4],
              pmat=persp.out),
      col="red", lwd=5)

## change in bodyfat as abdomin goes from 140 to 120, while
## holding biceps constant at 30
lines(trans3d(x=fat.lines$abdomin[c(3,5)],
              y=fat.lines$biceps[c(3,5)],
              z=fat.lines$bodyfat[c(3,5)],
              pmat=persp.out),
      col="orange", lwd=7)
## redraw blue line thinner so we can see orange line is
## a segment of the blue line at abdomin=140
lines(trans3d(x=fat.lines$abdomin[c(1,5)],
              y=fat.lines$biceps[c(1,5)],
              z=fat.lines$bodyfat[c(1,5)],
              pmat=persp.out),
      col="blue", lwd=1)

title(sub="
The ratio 0.6829379 of the length of the orange line to the red line is the
regression coefficient for abdomin: the change in predicted bodyfat when
abdomin is changed one unit holding biceps constant at 30.
")

diff(fat.lines$bodyfat[5:6]) / diff(fat.lines$abdomin[5:6])
coef(fat.lm)
