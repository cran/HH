"norm.setup" <-
  function(xlim.in=c(-2.5,2.5),
           ylim.in=c(0,.4)/se,
           mean=0,
           main.in=ifelse(
             !(missing(se) && missing(sd) && missing(n)),
             paste("normal density:  se =", round(se,3)),
             "Standard Normal Density N(0,1)"),
           se=sd/sqrt(n), sd=1, n=1, ...) {
  plot(xlim=xlim.in, ylim=ylim.in,
       x=xlim.in, y=ylim.in,
       yaxt="n", type="n",
       las=1,
       xlab="", ylab="f(z)",
       main=main.in)
  axis(4, las=1)
}

"norm.curve" <-
function(mean=0, se=sd/sqrt(n),
         critical.values=mean + se*c(-1.96, 1.96),
         z=do.call("seq", as.list(c((par()$usr[1:2]-mean)/se, length=109))),
         shade, col=par("col"), axis.name="z", sd=1, n=1, ...) {

  ## Valid values for shade are "right", "left", "inside", "outside".
  ## Default is "right" for one-sided critical.values and "outside"
  ## for two-sided critical values

  x.z <- mean + z*se
  lines(y=dnorm(z)/se, x=x.z)
  zvals <- trunc(range(z))
  zvals <- seq(zvals[1], zvals[2], 1)
  if (axis.name=="z") {
  if (!(missing(se) && missing(sd) && missing(n)))
      axis(1, at=mean+se*zvals, labels=zvals, tick=FALSE, line=2)
  }
  else
    axis(1, at=mean+se*zvals, labels=zvals, tick=FALSE, line=4)
  y.ticks <- pretty(par()$usr[3:4]*se)
  axis(2, at=y.ticks/se, labels=y.ticks, las=1)
  if (!(missing(se) && missing(sd) && missing(n)))
    mtext(side=4, text="f((xbar-m)/se) / se  =  f(z)/se", line=3, cex=par()$cex)
      
  critical.one <- TRUE
  if (length(critical.values)==1) {
    if (missing(shade) || shade=="right")
      x <- seq((critical.values-mean)/se, max(z), length=51)*se + mean
    else
      x <- seq(min(z), (critical.values-mean)/se, length=51)*se + mean
  }
  if (length(critical.values)==2) {
    if (missing(shade) || shade=="outside") {
      critical.one <- FALSE
      x1 <- seq(min(z), (critical.values[1]-mean)/se, length=51)*se + mean
      x2 <- seq((critical.values[2]-mean)/se, max(z), length=51)*se + mean
    }
    else
      x <- seq((critical.values[1]-mean)/se, (critical.values[2]-mean)/se, .1)*se + mean
  }
  if (critical.one)
    polygon(x=c(x[1], x, x[length(x)]),
            y=c(0, dnorm((x-mean)/se)/se, 0),
            col=col)
  else {
    polygon(x=c(x1[1], x1, x1[length(x1)]),
            y=c(0, dnorm((x1-mean)/se)/se, 0),
            col=col)
    polygon(x=c(x2[1], x2, x2[length(x2)]),
            y=c(0, dnorm((x2-mean)/se)/se, 0),
            col=col)
  }

  axis(1, at=critical.values, tck=-.15, labels=FALSE)
  left.margin <- .15*diff(par()$usr[1:2])
  if (axis.name=="z") {
    axis(1, at=critical.values, tick=FALSE, line=3,
         labels=round((critical.values-mean)/se, 3))
    if (!(missing(se) && missing(sd) && missing(n)))
      mtext(side=1, at=par()$usr[1]-left.margin, line=3, text="z", cex=par()$cex)
    else
      mtext(side=1, at=par()$usr[1]-left.margin, line=1, text="z", cex=par()$cex)
    mtext(side=1, at=par()$usr[1]-left.margin, line=4, text="z", cex=par()$cex)
    if (!(missing(se) && missing(sd) && missing(n))) {
      axis(1, at=critical.values, tick=FALSE, line=1,
         labels=round(critical.values, 3))
      mtext(side=1, at=par()$usr[1]-left.margin, line=1, text="xbar", cex=par()$cex)
      mtext(side=1, at=par()$usr[1]-left.margin, line=2, text="xbar", cex=par()$cex)
      mtext(side=3, at=par()$usr[1]-left.margin, line=.5, text="xbar mean", cex=par()$cex)
    }
    else
      mtext(side=3, at=par()$usr[1]-left.margin, line=.5, text="z mean", cex=par()$cex)

  }
  else { ## (axis.name=="z1")
    axis(1, at=critical.values, tick=FALSE, line=5,
         labels=round((critical.values-mean)/se, 3))
    mtext(side=1, at=par()$usr[1]-left.margin, line=5, text="z1", cex=par()$cex)
    mtext(side=1, at=par()$usr[1]-left.margin, line=6, text="z1", cex=par()$cex)
  }
  axis(1, at=mean, tck=-.04, labels="")
  axis(3, at=mean, tck=-.02, labels="", xpd=TRUE)
  axis(3, at=mean, tick=FALSE, xpd=TRUE, line=-.5, labels=round(mean, 3))
  abline(h=0, v=mean)
  invisible(NULL)
}
