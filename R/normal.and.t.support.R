## library(lattice)
## library(latticeExtra)

Base <- function(dfunction,
                 xlim,
                 ylim,
                 ylab=deparse(substitute(dfunction)),
                 xlab="x", ...,
                 par.settings=list(
                   clip=list(panel=FALSE),
                   layout.heights=list(key.axis.padding=key.axis.padding)
                 ),
                 key.axis.padding) {
  xyplot(ylim ~ xlim, type="n", ylab=ylab, xlab=xlab, ...,
         par.settings=par.settings)
}

Curve <- function(dfunction, xlow, xhigh,
                  col=NA, border="black", lwd=1,
                  base=FALSE, closed=TRUE, ..., mean=0, sd=1, df=Inf) {
  xx <- seq(xlow, xhigh, length=201)
  yy <- dfunction(xx, mean=mean, sd=sd, df=df)
  if (closed) {
    if (base) {
      xx <- c(xx[1], xx, xx[length(xx)])
      yy <- c(0,     yy, 0             )
    }
    layer(panel.polygon(x=xx,
                        y=yy,
                        col=col,
                        border=border, lwd=lwd),
          data=list(xx=xx, yy=yy, col=col, border=border, lwd=lwd))
  }
  else
        layer(panel.lines(x=xx,
                        y=yy,
                        col=border,
                        lwd=lwd),
          data=list(xx=xx, yy=yy, col=col, border=border, lwd=lwd))
}


Area <- function(..., border=NA) {
  Curve(..., border=border)
}

Border <- function(..., col=NA) {
  Curve(..., col=col, closed=FALSE)
}

Vertical <- function(v, lty=4, col="black", lwd=1) {
  layer(panel.abline(v=v, lty=lty, col=col, lwd=lwd),
        data=list(v=v, lty=lty, col=col, lwd=lwd))
}

Zeroline <- layer(panel.refline(h=0, col="gray70"))

AxisNormal <- function(side="top", at, labels=TRUE, outside=TRUE, rot=0,
                       line.col="black", text.col="black", tck=1, text.cex=1, ...) {
  layer(panel.axis(side=side, at=at, labels=labels, outside=outside, rot=rot,
                   line.col=line.col, text.col=text.col, tck=tck, text.cex=text.cex),
        data=list(side=side, at=at, labels=labels, outside=outside, rot=rot,
          line.col=line.col, text.col=text.col, tck=tck, text.cex=text.cex))
}


globalVariables(c('mu', 'bar', 'x', 'sigma', 'nu', 's'))
Main <- function(mean0, mean1, xbar, sd, n, df) {

  argnames <- alist(mu0=mu[0], mu1=mu[1], xbarsymbol=bar(x),
                    sdsymbol=sigma, nn=n, dfdf=nu, sesymbol=sigma[bar(x)],
                    sdsymbolt=s, sesymbolt=s[bar(x)])
  argvals <- list(mean0=mean0, mean1=mean1, xbar=xbar,
                  sd=sd, n=n, df=df, se=format(sd/sqrt(n), digits=2))
  argsboth <- c(argnames, argvals)

##  if (is.na(xbar) && is.infinite(df)) {
    ms      <- substitute("Normal: " * mu0==mean0 * ", " ~                                               sdsymbol==sd                                  , argsboth)
    mssn    <- substitute("Normal: " * mu0==mean0 * ", " ~                                               sesymbol==se  * ", " ~ nn==n                  , argsboth)
    mms     <- substitute("Normal: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~                           sdsymbol==sd                                  , argsboth)
    mmssn   <- substitute("Normal: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~                           sesymbol==se  * ", " ~ nn==n                  , argsboth)
##  }

##  if (!is.na(xbar) && is.infinite(df)) {
    msx     <- substitute("Normal: " * mu0==mean0 * ", " ~                     xbarsymbol==xbar * ", " ~ sdsymbol==sd                                  , argsboth)
    mssnx   <- substitute("Normal: " * mu0==mean0 * ", " ~                     xbarsymbol==xbar * ", " ~ sesymbol==se  * ", " ~ nn==n                  , argsboth)
    mmsx    <- substitute("Normal: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~ xbarsymbol==xbar * ", " ~ sdsymbol==sd                                  , argsboth)
    mmssnx  <- substitute("Normal: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~ xbarsymbol==xbar * ", " ~ sesymbol==se  * ", " ~ nn==n                  , argsboth)
##  }

##  if (is.na(xbar) && !is.infinite(df)) {
    msd     <- substitute("t: " * mu0==mean0 * ", " ~                                               sdsymbolt==sd                * ", " ~ dfdf==df, argsboth)
    mssnd   <- substitute("t: " * mu0==mean0 * ", " ~                                               sesymbolt==se * ", " ~ nn==n * ", " ~ dfdf==df, argsboth)
    mmsd    <- substitute("t: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~                           sdsymbolt==sd                * ", " ~ dfdf==df, argsboth)
    mmssnd  <- substitute("t: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~                           sesymbolt==se * ", " ~ nn==n * ", " ~ dfdf==df, argsboth)
##  }

##  if (!is.na(xbar) && !is.infinite(df)) {
    msxd    <- substitute("t: " * mu0==mean0 * ", " ~                     xbarsymbol==xbar * ", " ~ sdsymbolt==sd                * ", " ~ dfdf==df, argsboth)
    mssnxd  <- substitute("t: " * mu0==mean0 * ", " ~                     xbarsymbol==xbar * ", " ~ sesymbolt==se * ", " ~ nn==n * ", " ~ dfdf==df, argsboth)
    mmsxd   <- substitute("t: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~ xbarsymbol==xbar * ", " ~ sdsymbolt==sd                * ", " ~ dfdf==df, argsboth)
    mmssnxd <- substitute("t: " * mu0==mean0 * ", " ~ mu1==mean1 * ", " ~ xbarsymbol==xbar * ", " ~ sesymbolt==se * ", " ~ nn==n * ", " ~ dfdf==df, argsboth)
##  }

  mains <-
    array(c(ms,   mssn,   mms,   mmssn,
            msx,  mssnx,  mmsx,  mmssnx,
            msd,  mssnd,  mmsd,  mmssnd,
            msxd, mssnxd, mmsxd, mmssnxd),
          dim=c(2, 2, 2, 2),
          dimnames=list(
            n=c("1","many"), mean1=c("0","01"),
            xbar=c("na","xbar"), df=c("z","t")))

  as.expression(mains[
      2, ## 1 + (n > 1),
      1 + !is.na(mean1),
      1 + !is.na(xbar),
      1 + !is.infinite(df)
      ])

}



Float <- function(sided, type,
                  xbarc.left, xbarc.right, xbar,
                  alpha, beta, power, pvalue,
                  conf,
                  xlim, ylim,
                  mean0, mean1,
                  col.alpha, col.beta, col.power, col.pvalue, col.conf, cex.prob=.6,
                  prob.labels,
                  xhalf.multiplier, digits) {
  labels <- if (prob.labels)
              as.expression(
                c(substitute(symbol==value, c(alist(symbol=alpha),  list(value=round(alpha,  digits)))),
                  substitute(symbol==value, c(alist(symbol=beta),   list(value=round(beta,   digits)))),
                  substitute(symbol==value, c(alist(symbol=1-beta), list(value=round(power,  digits)))),
                  substitute(symbol==value, c(alist(symbol="p"),      list(value=round(pvalue, digits)))),
                  substitute(symbol==value, c(alist(symbol="Conf"),   list(value=round(conf,   digits)))))
              )
            else
              c(alp=round(alpha, digits),
                bet=round(beta, digits),
                pow=round(power, digits),
                pva=round(pvalue, digits),
                cnf=round(conf, digits))

  if (sided=="left" ||
      (sided=="both" && (!is.na(mean1) && mean1 < mean0) || (!is.na(xbar) && xbar < mean0))
      ) {
    AddSubtract  <- c(-1, 1, -1, -1.5, 1.5)*2.5
    xbarc <- xbarc.left
  } else {
    AddSubtract  <- c(1, -1, 1, 1.5, -1.5)*2.5
    xbarc <- xbarc.right
  }


  xvalues  <- c(alpha=xbarc, beta=xbarc, power=xbarc, pvalue=xbar, conf=xbarc) +
    AddSubtract * diff(xlim)/40
  yvalues <- c(alpha=3, beta=5, power=7, pvalue=1, conf=3) * diff(ylim)/30

  border <- c(col.alpha=col.alpha,
              col.beta=col.beta,
              col.power=col.power,
              col.pvalue=col.pvalue,
              col.conf=col.conf)
  xhalf <- if (prob.labels)
             cex.prob * diff(xlim)/9 * xhalf.multiplier
           else
             cex.prob * diff(xlim)/15 *xhalf.multiplier
  yhalf <- cex.prob * diff(ylim)/30

  subscripts <- c(alpha=(type=="hypothesis"),
                  beta=(type=="hypothesis") && (!is.na(mean1)),
                  power=(type=="hypothesis") && (!is.na(mean1)),
                  pvalue=(type=="hypothesis") && (!is.na(xbar)),
                  conf=(type=="confidence"))
  xvalues <- xvalues [subscripts]
  yvalues <- yvalues [subscripts]
  labels  <- labels  [subscripts]
  border  <- border  [subscripts]

  layer({
    panel.rect(xvalues-xhalf, yvalues-yhalf, xvalues+xhalf, yvalues+yhalf,
               col="white", border=border, lwd=2)
    old.digits <- options(digits=digits)
    panel.text(xvalues, yvalues, labels=labels, cex=cex.prob)
    options(old.digits)
  }, data=list(
       xvalues=xvalues, yvalues=yvalues, labels=labels, border=border,
       xhalf=xhalf, yhalf=yhalf, cex.prob=cex.prob, digits=digits)
        )
}



MainSimpler <- function(mean0, mean1, xbar, sd, n, df, distribution.name, digits) {

  argnames <- alist(mu0=mu[0], mu1=mu[1], xbarsymbol=bar(x),
                    sdsymbol=sigma, nn=n, dfdf=nu, sesymbol=sigma[bar(x)],
                    sdsymbolt=s, sesymbolt=s[bar(x)])
  argvals <- list(mean0=mean0, mean1=mean1, xbar=xbar,
                  sd=sd, n=n, df=df, se=format(sd/sqrt(n), digits=digits))
  argsboth <- c(argnames, argvals)

  if (distribution.name == "t")
    main <- substitute("t: " * sesymbolt==se * ", " ~ nn==n * ", " ~ dfdf==df, argsboth)
  else
    main <- substitute("normal: " * sesymbol==se * ", " ~ nn==n, argsboth)

  as.expression(main)
}






ColorWithAlpha <- function(colorname, alpha=127) { ## designed for scalar color name from colors()
  rgb(t(col2rgb(colorname)), maxColorValue=255, alpha=alpha)
}
