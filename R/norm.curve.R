"norm.setup" <-
  function(xlim.in=c(-2.5,2.5),
           ylim.in=c(0,.4)/se,
           mean=0,
           main.in=main.calc,
           se=sd/sqrt(n), sd=1, n=1,
           df.t=NULL,
           ...) {
    main.calc <-
      if (is.null(df.t) || df.t==Inf)  ## normal
        ifelse(!(missing(se) && missing(sd) && missing(n)),
               paste("normal density:  se =", round(se,3)),
               "Standard Normal Density N(0,1)")
      else { ## t distribution
        if (length(df.t) != 1) stop("df.t must have length 1")
        ifelse(!(missing(se) && missing(sd) && missing(n)),
               paste("t density:  se = ", round(se,3), ", df = ", df.t, sep=""),
               paste("t density, df =", df.t))
      }
    plot(xlim=xlim.in, ylim=ylim.in,
         x=xlim.in, y=ylim.in,
         yaxt="n", type="n",
         las=1,
         xlab="",
         ylab=ifelse(is.null(df.t) || df.t==Inf, "f(z)", "f(t)"),
         main=main.in)
    axis(4, las=1)
}

"norm.curve" <-
function(mean=0, se=sd/sqrt(n),
         critical.values=mean + se*c(-1, 1)*z.975,
         z=do.call("seq", as.list(c((par()$usr[1:2]-mean)/se, length=109))),
         shade, col=par("col"),
         axis.name=ifelse(is.null(df.t) || df.t==Inf, "z", "t"),
         second.axis.label.line=3,
         sd=1, n=1,
         df.t=NULL,
         ...) {

  ## Valid values for shade are "right", "left", "inside", "outside",
  ## "none".  Default is "right" for one-sided critical.values and
  ## "outside" for two-sided critical values.  "none" is used to
  ## redraw an outline of the curve that would otherwise be obscured
  ## by a solid color from the shaded area of another curve.

  z.975 <- if (is.null(df.t) || df.t==Inf) qnorm(.975) else qt(.975, df.t)

  if (missing(shade))
    shade <- switch(length(critical.values)+1,
                    "none",
                    "right",
                    "outside",
                    stop("Specify no more than 2 critical values."))

  cex.small <- par()$cex*.7
  
  z.critical.values <- (critical.values-mean)/se
  
  dfunction <-  function(z, df.t=NULL)
    if (is.null(df.t) || df.t==Inf) dnorm(z) else dt(z, df.t)
  pfunction <-  function(z, df.t=NULL)
    if (is.null(df.t) || df.t==Inf) pnorm(z) else pt(z, df.t)
    
  x.z <- mean + z*se
  lines(y=dfunction(z, df.t)/se, x=x.z)
  zvals <- trunc(range(z))
  zvals <- seq(zvals[1], zvals[2], 1)
  if (axis.name=="z" || axis.name=="t") {
    if (!(missing(se) && missing(sd) && missing(n))) {
      axis.list <- list(1, at=mean+se*zvals, labels=zvals, tick=FALSE,
                        line=2)
      if.R(r=axis.list$cex.axis <- cex.small,
           s=axis.list$cex      <- cex.small)
      do.call("axis", axis.list)
    }
  }
  else {
    axis.list <- list(1, at=mean+se*zvals, labels=zvals, tick=FALSE,
                      line=4)
      if.R(r=axis.list$cex.axis <- cex.small,
           s=axis.list$cex      <- cex.small)
      do.call("axis", axis.list)
  }
  y.ticks <- pretty(par()$usr[3:4]*se)
  axis(2, at=y.ticks/se, labels=y.ticks, las=1)
  if (!(missing(se) && missing(sd) && missing(n))) {
    mtext(side=4,
          text=ifelse(is.null(df.t) || df.t==Inf,
            "f((xbar-m)/se) / se  =  f(z)/se",
            "f((xbar-m)/se) / se  =  f(t)/se"),
          line=second.axis.label.line, cex=par()$cex,
          col=ifelse(second.axis.label.line==3, 1, col))
    mtext(side=2,
          text=ifelse(is.null(df.t) || df.t==Inf,
            "f(z)",
            "f(t)"),
          line=second.axis.label.line, cex=par()$cex,
          col=ifelse(second.axis.label.line==3, 1, col))
  }
  critical.one <- TRUE
  if (length(critical.values)==1) {
    if (shade=="right") {
      x <- seq(z.critical.values, max(z), length=51)*se + mean
      shaded.area <- 1-pfunction(z.critical.values, df.t)
    }
    else {
      x <- seq(min(z), z.critical.values, length=51)*se + mean
      shaded.area <- pfunction(z.critical.values, df.t)
    }
  }
  if (length(critical.values)==2) {
    if (shade=="outside") {
      critical.one <- FALSE
      x1 <- seq(min(z), (critical.values[1]-mean)/se, length=51)*se + mean
      x2 <- seq(z.critical.values[2], max(z), length=51)*se + mean
      shaded.area <- 1-diff(pfunction(z.critical.values, df.t))
    }
    else { ## shade == "inside"
      x <- seq(z.critical.values[1], z.critical.values[2], .1)*se + mean
      shaded.area <- diff(pfunction(z.critical.values, df.t))
    }
  }
  if (shade != "none") {
    if (critical.one)
      polygon(x=c(x[1], x, x[length(x)]),
              y=c(0, dfunction((x-mean)/se, df.t)/se, 0),
              col=col)
    else {
      polygon(x=c(x1[1], x1, x1[length(x1)]),
              y=c(0, dfunction((x1-mean)/se, df.t)/se, 0),
              col=col)
      polygon(x=c(x2[1], x2, x2[length(x2)]),
              y=c(0, dfunction((x2-mean)/se, df.t)/se, 0),
              col=col)
    }
  }

  axis(1, at=critical.values, tck=-.09, labels=FALSE)
  left.margin <- .15*diff(par()$usr[1:2])
  if (axis.name=="z" || axis.name=="t") {
    axis(1, at=critical.values, tick=FALSE, line=3,
         labels=round(z.critical.values, 3))
    if (!(missing(se) && missing(sd) && missing(n)))
      mtext(side=1, at=par()$usr[1]-left.margin, line=3, text=axis.name, cex=cex.small)
    else
      mtext(side=1, at=par()$usr[1]-left.margin, line=1, text=axis.name, cex=cex.small)
    mtext(side=1, at=par()$usr[1]-left.margin, line=4, text=axis.name, cex=cex.small)
    if (shade != "none") {
      mtext(side=1, at=par()$usr[2]+left.margin, line=1,
            text="shaded area", cex=par()$cex)
      mtext(side=1, at=par()$usr[2]+left.margin, line=4,
            text=format(shaded.area, digits=3), cex=par()$cex, col=col)
    }
    if (!(missing(se) && missing(sd) && missing(n))) {
      axis(1, at=critical.values, tick=FALSE, line=1,
         labels=round(critical.values, 3))
      mtext(side=1, at=par()$usr[1]-left.margin, line=1, text="xbar", cex=cex.small)
      mtext(side=1, at=par()$usr[1]-left.margin, line=2, text="xbar", cex=cex.small)
      mtext(side=3, at=par()$usr[1]-left.margin, line=.5, text="xbar mean", cex=par()$cex)
    }
    else
      mtext(side=3, at=par()$usr[1]-left.margin, line=.5,
            text=paste(axis.name, "mean"),
            cex=par()$cex)

  }
  else { ## (axis.name=="z1" || axis.name=="t1")
    axis(1, at=critical.values, tick=FALSE, line=5,
         labels=round(z.critical.values, 3))
    mtext(side=1, at=par()$usr[1]-left.margin, line=5,
          text=axis.name, cex=cex.small)
    mtext(side=1, at=par()$usr[1]-left.margin, line=6,
          text=axis.name, cex=cex.small)
    if (shade != "none")
      mtext(side=1, at=par()$usr[2]+left.margin, line=6,
            text=format(shaded.area, digits=3), cex=par()$cex, col=col)
  }
  axis(1, at=mean, tck=-.04, labels="")
  axis(3, at=mean, tck=-.02, labels="", xpd=TRUE)
  axis(3, at=mean, tick=FALSE, xpd=TRUE, line=-.5, labels=round(mean, 3))
  abline(h=0, v=mean)
  invisible(NULL)
}

norm.observed <- function(xbar, t.xbar, col="blue") {
  abline(v=xbar, col=col, lty=5)
  arrows(xbar, par()$usr[3:4]+c(-.01,.01), xbar, par()$usr[3:4],
         xpd=TRUE, col=col, length=.1)
  axis(side=1, at=xbar, label=FALSE, col=col)
  axis(side=3, at=xbar, label=FALSE, col=col)
  mtext(side=3, text=round(xbar,3), at=xbar, line=.5, cex=par()$cex, col=col)
  mtext(side=1, text=round(t.xbar,3), at=xbar, line=4.5, cex=par()$cex/2, col=col)
}
