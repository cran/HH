NormalAndTplot <- function(mean0, ...)
  UseMethod("NormalAndTplot")


NormalAndTplot.default <- function(mean0=0,
                                   mean1=NA,
                                   xbar=NA,
                                   sd=1, df=Inf, n=1,
                                   xlim=c(-3, 3), ylim, alpha.right=.05, alpha.left=0,
                                   float=TRUE, ntcolors="original",
                                   digits=4, digits.axis=digits, digits.float=digits,
                                   distribution.name=c("normal","z","t"),
                                   type=c("hypothesis", "confidence"),
                                   zaxis=FALSE, z1axis=FALSE,
                                   cex.z=.5, cex.prob=.6, cex.top.axis=1,
                                   main, xlab, ylab,
                                   prob.labels=(type=="hypothesis"),
                                   xhalf.multiplier=1,
                                   cex.main=1,
                                   key.axis.padding=4.5, ...) {

  type <- match.arg(type)
  if (type == "confidence") {
    if (!is.na(xbar)) mean0 <- xbar
    if (is.na(xbar)) xbar <- mean0
  }

  if (is.list(zaxis)) {
    zaxis.list <- zaxis
    if (!all(c("at", "labels") %in% names(zaxis)))
      stop("The list 'zaxis' must contain 'at' and 'labels' components.", call.=FALSE)
    zaxis <- TRUE
  } else zaxis.list <- list()

  if (is.list(z1axis)) {
    z1axis.list <- z1axis
    if (!all(c("at", "labels") %in% names(z1axis)))
      stop("The list 'z1axis' must contain 'at' and 'labels' components.", call.=FALSE)
    z1axis <- TRUE
  } else z1axis.list <- list()

  distribution.name <- match.arg(distribution.name)
  if (distribution.name=="t" && is.infinite(df)) distribution.name <- "normal"
  if ((distribution.name=="z" || distribution.name=="normal") &&
      !is.infinite(df)) df <- Inf
  stderr <- sd/sqrt(n)
  switch(distribution.name,
         normal=,
         z={
           dfunction <- function(x, df=df, mean=mean, sd=stderr, ...) dnorm(x, mean=mean, sd=sd)
           qfunction <- function (..., df) qnorm(...)
           pfunction <- function (..., df) pnorm(...)
         },
         t={
           dfunction <- function(x, df=df, mean=mean, sd=stderr, ...) dt(x=(x-mean)/sd, df=df) / sd
           qfunction <- qt
           pfunction <- pt
         }
         )

  green127 <- ColorWithAlpha("green")
  blue127 <- ColorWithAlpha("blue")
  black127 <- ColorWithAlpha("black")

  if (ntcolors == "original") {
    col.alpha             <- "blue"
    col.notalpha          <- "lightblue"
    col.beta              <- "red"
    col.power             <- "pink"
    col.pvalue            <- "green"
    col.pvaluetranslucent <- green127
    col.critical          <- "gray50"
    col.border            <- black127
    col.text              <- "black"
    col.conf              <- "lightgreen"
  }
  if (ntcolors == "stoplight") {
    col.alpha             <- "red"
    col.notalpha          <- "honeydew2"
    col.beta              <- "orange"
    col.power             <- "pink"
    col.pvalue            <- "blue"
    col.pvaluetranslucent <- blue127
    col.critical          <- "gray50"
    col.border            <- black127
    col.text              <- "black"
    col.conf              <- "lightgreen"
  }

  if (type == "confidence") {
    col.alpha <- "white"
    col.notalpha <- col.conf
  }


  if (missing(ylim)) ylim <- c(0, dfunction(x=mean0, mean=mean0, sd=stderr, df=df) * 1.04)

  distribution.name.type <- paste(distribution.name, type, sep=".")

  if (missing(ylab))
    ylab <- switch(distribution.name.type,
                   normal.hypothesis=,
                   z.hypothesis=list(expression(phi(scriptstyle(over(bar(x)-mu[i], sigma[~bar(x)])))/scriptscriptstyle(sigma[~bar(x)])), cex=1.5, rot=0),
                   t.hypothesis=list(expression(t[nu]*(scriptstyle(over(bar(x)-mu[i], s[~bar(x)])))/scriptscriptstyle(s[~bar(x)])), cex=1.5, rot=0),
                   normal.confidence=,
                   z.confidence=list(expression(phi(scriptstyle(over(bar(x)-mu, sigma[~bar(x)])))/scriptscriptstyle(sigma[~bar(x)])), cex=1.5, rot=0),
                   t.confidence=list(expression(t[nu]*(scriptstyle(over(bar(x)-mu, s[~bar(x)])))/scriptscriptstyle(s[~bar(x)])), cex=1.5, rot=0))

  if (missing(xlab)) xlab <- expression(bar(x))

  ##  main <- Main(mean0, mean1, xbar, sd, n, df)
  if (missing(main))
    main <- list(MainSimpler(mean0, mean1, xbar, sd, n, df, distribution.name, digits=digits.axis), cex=cex.main)

  Setup <- Base(dfunction, xlim, ylim,
                ylab=ylab,
                xlab=xlab,
                main=main, ...,
                key.axis.padding=key.axis.padding)
  Setup.xlim <- Setup$x.limits
  Setup.ylim <- Setup$y.limits


  zc.right <- qfunction(p=alpha.right, lower=FALSE, df=df)
  xbarc.right <- zc.right * stderr + mean0
  if (is.infinite(xbarc.right)) xbarc.right <- Setup.xlim[2]
  zc.left <- qfunction(p=alpha.left, lower=TRUE, df=df)
  xbarc.left <- zc.left * stderr + mean0
  if (is.infinite(xbarc.left)) xbarc.left <- Setup.xlim[1]
## recover()
  Border0 <- Border(dfunction, Setup.xlim[1], Setup.xlim[2], base=TRUE, border=col.border, mean=mean0, sd=stderr, df=df)
  Border1 <- Border(dfunction, Setup.xlim[1], Setup.xlim[2], base=TRUE, border=col.border, mean=mean1, sd=stderr, df=df)

  Area0.left   <- Area(dfunction, Setup.xlim[1], xbarc.left, base=TRUE, col=col.alpha, mean=mean0, sd=stderr, df=df)
  Area0.middle <- Area(dfunction, xbarc.left, xbarc.right, base=TRUE, col=col.notalpha, mean=mean0, sd=stderr, df=df)
  Area0.right  <- Area(dfunction, xbarc.right, Setup.xlim[2], base=TRUE, col=col.alpha, mean=mean0, sd=stderr, df=df)
  Middle0 <- Vertical(mean0, col=col.notalpha, lwd=4)

  Area1.left   <- Area(dfunction, Setup.xlim[1], xbarc.left, base=TRUE, col=col.power, mean=mean1, sd=stderr, df=df)
  Area1.middle <- Area(dfunction, xbarc.left, xbarc.right, base=TRUE, col=col.beta, mean=mean1, sd=stderr, df=df)
  Area1.right  <- Area(dfunction, xbarc.right, Setup.xlim[2], base=TRUE, col=col.power, mean=mean1, sd=stderr, df=df)
  Middle1 <- Vertical(mean1, col=col.power, lwd=2)

  sided <- "both"
  if (alpha.left > 0 && alpha.right == 0) sided <- "left"
  if (alpha.left == 0 && alpha.right > 0) sided <- "right"
  ## if (alpha.right > 0 && alpha.left  > 0) sided <- "both"

  if (!is.na(xbar)) {
    if (sided == "left") {
      xbar.left <- xbar
      xbar.right <- Inf ## Setup.xlim[2]
      xbar.otherside <- Inf ## xbar  ## place holder
    }
    if (sided == "right") {
      xbar.left <- -Inf ## Setup.xlim[1]
      xbar.right <- xbar
      xbar.otherside <- -Inf ##xbar  ## place holder
    }
    if (sided == "both") {
      if (xbar >= mean0) {
        xbar.left <- mean0 - (xbar - mean0)
        xbar.right <- xbar
        xbar.otherside <- xbar.left
      }
      else {
        xbar.left <- xbar
        xbar.right <- mean0 + (mean0 - xbar)
        xbar.otherside <- xbar.right
      }
    }


    Empty <- layer(panel.points(x=vector(), y=vector()))

    Borderxbar.left <- if (is.infinite(xbar.left))
                         Empty
                       else
                         Border(dfunction, Setup.xlim[1], xbar.left, base=TRUE, border=col.pvalue, lwd=2, mean=mean0, sd=stderr, df=df)
    Borderxbar.right <- if (is.infinite(xbar.right))
                          Empty
                        else
                          Border(dfunction, xbar.right, Setup.xlim[2], base=TRUE, border=col.pvalue, lwd=2, mean=mean0, sd=stderr, df=df)
    Vertical.xbar <- Vertical(xbar, lty=2, lwd=2, col.pvalue)
    Vertical.xbar.otherside <- Vertical(xbar.otherside, lty=2, lwd=1, col.pvalue)
    Areaxbar.left <-  if (is.infinite(xbar.left))
                        Empty
                      else
                        Area(dfunction, Setup.xlim[1], xbar.left, base=TRUE, col=col.pvaluetranslucent, mean=mean0, sd=stderr, df=df)
    Areaxbar.right <- if (is.infinite(xbar.right))
                        Empty
                      else
                        Area(dfunction, xbar.right, Setup.xlim[2], base=TRUE, col=col.pvaluetranslucent, mean=mean0, sd=stderr, df=df)

    phantom <- function() {} ## placeholder to prevent R CMD check from complaining
    xbar.expr <- as.expression(substitute(xbarsymbol==xbar, c(alist(xbarsymbol=bar(x)[phantom("")]), list(xbar=format(xbar, digits=digits.axis)))))
    xbar.otherside.expr <- as.expression(substitute(xbarsymbol==xbar.otherside, c(alist(xbarsymbol=bar(x)["otherside"]), list(xbar.otherside=format(xbar.otherside, digits=digits.axis)))))
    Axis.xbar <- AxisNormal(at=xbar, labels=xbar.expr, line.col=col.pvalue, text.col=col.text, line.lwd=1, tck=5*cex.top.axis, text.cex=cex.top.axis) +
      AxisNormal(side="bottom", at=xbar, labels=format(xbar, digits=digits.axis), line.col=col.pvalue, text.col="transparent", line.lwd=1)
    Axis.xbar.otherside <- AxisNormal(at=xbar.otherside, labels=xbar.otherside.expr, line.col=col.pvalue, text.col=col.text, line.lwd=1, tck=5*cex.top.axis, text.cex=cex.top.axis) +
      AxisNormal(side="bottom", at=xbar.otherside, labels=format(xbar.otherside, digits=digits.axis), line.col=col.pvalue, text.col="transparent", line.lwd=1)
  }
  else
    {
        xbar.left <- NA
        xbar.right <- NA
        xbar.otherside <- NA
    }

  if (type == "hypothesis") {
    mean0.alist <-  alist(mu0=mu[0])
    xbarc.left.alist <- alist(xbarsymbol=bar(x)[c])
    xbarc.right.alist <- alist(xbarsymbol=bar(x)[c])
  }
  else {
    mean0.alist <-  alist(mu0=bar(x))
    xbarc.left.alist <- alist(xbarsymbol=mu["LCL"])
    xbarc.right.alist <- alist(xbarsymbol=mu["UCL"])
  }
  mean0.expr <- as.expression(substitute(mu0==mean0, c(mean0.alist, list(mean0=format(mean0, digits=digits.axis)))))
  mean1.expr <- as.expression(substitute(mu1==mean1, c(alist(mu1=mu[1]), list(mean1=format(mean1, digits=digits.axis)))))
  Axis.0 <- AxisNormal(at=mean0, labels=mean0.expr, line.col=col.notalpha, line.lwd=2, tck=1*cex.top.axis, text.cex=cex.top.axis) +
    AxisNormal(side="bottom", at=mean0, labels=format(mean0, digits=digits.axis), line.col=col.notalpha, text.col="transparent", line.lwd=2)
  Axis.1 <- AxisNormal(at=mean1, labels=mean1.expr, line.col=col.power, line.lwd=2, tck=1*cex.top.axis, text.cex=cex.top.axis) +
    AxisNormal(side="bottom", at=mean1, labels=format(mean1, digits=digits.axis), line.col=col.power, text.col="transparent", line.lwd=2)
  xbarc.left.expr <-  as.expression(substitute(xbarsymbol==xbar, c(xbarc.left.alist, list(xbar=format(xbarc.left, digits=digits.axis)))))
  xbarc.right.expr <- as.expression(substitute(xbarsymbol==xbar, c(xbarc.right.alist, list(xbar=format(xbarc.right, digits=digits.axis)))))
  Axis.xbarc.right <- AxisNormal(at=xbarc.right, labels=xbarc.right.expr, line.col=col.critical, text.col=col.text, tck=3.00*cex.top.axis, text.cex=cex.top.axis) +
    AxisNormal(side="bottom", at=xbarc.right, labels=format(xbarc.right, digits=digits.axis), line.col=col.critical, text.col="transparent", line.lwd=1.25)
  Axis.xbarc.left <- AxisNormal(at=xbarc.left, labels=xbarc.left.expr, line.col=col.critical, text.col=col.text, tck=3.00*cex.top.axis, text.cex=cex.top.axis) +
    AxisNormal(side="bottom", at=xbarc.left, labels=format(xbarc.left, digits=digits.axis), line.col=col.critical, text.col="transparent", line.lwd=1.25)
  Vertical.xbarc.left <- Vertical(xbarc.left, col=col.critical, lty=2)
  Vertical.xbarc.right <- Vertical(xbarc.right, col=col.critical, lty=2)


 ## recover()

  if (!is.na(xbar)) {
    pvalue.right <- if (sided != "left") pfunction((xbar.right-mean0)/stderr, df=df, lower=FALSE) else NA
    pvalue.left <- if (sided != "right") pfunction((xbar.left-mean0)/stderr, df=df, lower=TRUE) else NA
    pvalue <- switch(sided,
                     right=pvalue.right,
                     left= pvalue.left,
                     both= pvalue.right + pvalue.left)
  } else {
    pvalue.right <- NA
    pvalue.left <- NA
    pvalue <- NA
  }

  if (!is.na(mean1)) {
    power.right <- if (sided != "left") pfunction((xbarc.right-mean1)/stderr, df=df, lower=FALSE) else NA
    power.left <- if (sided != "right") pfunction((xbarc.left-mean1)/stderr, df=df, lower=TRUE) else NA
    power <- switch(sided,
                    right=power.right,
                    left= power.left,
                    both= power.right + power.left)
  } else {
    power.right <- NA
    power.left <- NA
    power <- NA
  }

  Floats <- Float(sided, type,
                  xbarc.left, xbarc.right, xbar,
                  alpha=alpha.left + alpha.right,
                  beta=1-power,
                  power=power,
                  pvalue=pvalue,
                  conf=1-(alpha.left + alpha.right),
                  Setup.xlim, Setup.ylim,
                  mean0, mean1,
                  col.alpha, col.beta, col.power, col.pvalue, col.conf, cex.prob,
                  prob.labels,
                  xhalf.multiplier, digits=digits.float)



  if (type == "hypothesis") {
    result <- Setup
    if (sided != "right") result <- result + Vertical.xbarc.left
    if (sided != "left")  result <- result + Vertical.xbarc.right
    result <- result + Area0.middle + Middle0
    if (!is.na(mean1)) {
      result <- result + Area1.middle
      if (sided != "right") {
        if (power.left > alpha.left)
          result <- result + Area1.left + Area0.left
        else
          result <- result + Area0.left + Area1.left
      }
      if (sided != "left") {
        if (power.right > alpha.right)
          result <- result + Area1.right + Area0.right
        else
          result <- result + Area0.right + Area1.right
      }
      result <- result + Middle1 + Border0
    }
    else
      result <- result + Area0.left + Area0.right + Border0
    ## if (!is.na(mean1)) result <- result + Area1.left + Area1.middle + Area1.right + Middle1
    ## result <- result + Area0.left + Area0.right + Border0
    if (!is.na(mean1)) result <- result + Border1
    if (!is.na(xbar)) {
      result <- result + Borderxbar.left + Borderxbar.right + Axis.xbar + Vertical.xbar + Areaxbar.left + Areaxbar.right
      result <- result + Middle1
    }
    result <- result + Zeroline
    if (!is.na(xbar) && sided == "both") result <- result + Axis.xbar.otherside + Vertical.xbar.otherside
    if (sided != "right") result <- result + Axis.xbarc.left
    if (sided != "left")  result <- result + Axis.xbarc.right
    if (!is.na(mean1)) result <- result + Axis.1
    result <- result + Axis.0
    if (float) result <- result + Floats
    if (zaxis) {
      if (length(zaxis.list) == 0) {
        z.pretty <- pretty((Setup.xlim - mean0)/stderr)
        z.at <- stderr * z.pretty + mean0
        z.labels <- signif(z.pretty, digits)
      }
      else {
        z.at <- zaxis.list$at
        z.labels <- zaxis.list$labels
      }
      result <- result + layer(
        {
          panel.axis("bottom", outside=TRUE, tck=2+cex.z, at=z.at, labels=z.labels,
                     text.cex=cex.z, rot=0, line.col="transparent")
          panel.text(x=convertX(unit(-3,   "strwidth",  data="z"), unitTo="native", valueOnly=TRUE),
                     y=convertY(unit(-2.5, "strheight", data="z"), unitTo="native", valueOnly=TRUE),
                     ifelse(distribution.name=="t", "t", "z"), cex=cex.z)
        },
        data=list(z.at=z.at, z.labels=z.labels, distribution.name=distribution.name, cex.z=cex.z))

      if (z1axis && !is.na(mean1)) {
        if (length(z1axis.list) == 0) {
          z1.pretty <- pretty((Setup.xlim - mean1)/stderr)
          z1.at <- stderr * z1.pretty + mean1
          z1.labels <- signif(z1.pretty, digits)
        }
        else {
          z1.at <- z1axis.list$at
          z1.labels <- z1axis.list$labels
        }
        result <- result + layer(
          {
            panel.axis("bottom", outside=TRUE, tck=3+2*cex.z, at=z1.at, labels=z1.labels,
                       text.cex=cex.z, rot=0, line.col="transparent")
            panel.text(x=convertX(unit(-3,   "strwidth",  data="z"), unitTo="native", valueOnly=TRUE),
                       y=convertY(unit(-3.2, "strheight", data="z"), unitTo="native", valueOnly=TRUE),
                       ifelse(distribution.name=="t", expression(t[1]), expression(z[1])), cex=cex.z)
          },
          data=list(z1.at=z1.at, z1.labels=z1.labels, distribution.name=distribution.name, cex.z=cex.z))
      }
    }
  }
  else { ## confidence interval
    result <- Setup
    if (sided != "right") result <- result + Vertical.xbarc.left
    if (sided != "left")  result <- result + Vertical.xbarc.right
    result <- result + Area0.middle + Middle0
    result <- result + Area0.middle + Border0
    result <- result + Zeroline
    if (sided != "right") result <- result + Axis.xbarc.left
    if (sided != "left")  result <- result + Axis.xbarc.right
    result <- result + Axis.0
    if (float) result <- result + Floats
    if (zaxis) {
      if (length(zaxis.list) == 0) {
        z.pretty <- pretty((Setup.xlim - mean0)/stderr)
        z.at <- stderr * z.pretty + mean0
        z.labels <- signif(z.pretty, digits)
      }
      else {
        z.at <- zaxis.list$at
        z.labels <- zaxis.list$labels
      }
      result <- result + layer(
        {
          panel.axis("bottom", outside=TRUE, tck=2+cex.z, at=z.at, labels=z.labels,
                     text.cex=cex.z, rot=0, line.col="transparent")
          panel.text(x=convertX(unit(-3,   "strwidth",  data="z"), unitTo="native", valueOnly=TRUE),
                     y=convertY(unit(-2.5, "strheight", data="z"), unitTo="native", valueOnly=TRUE),
                     ifelse(distribution.name=="t", "t", "z"), cex=cex.z)
        },
        data=list(z.at=z.at, z.labels=z.labels, distribution.name=distribution.name, cex.z=cex.z))
    }
  }

  result.table <- NormalAndT.table(distribution.name= distribution.name,
                                     type=              type,
                                     mean0=             mean0,
                                     mean1=             mean1,
                                     xbar=              xbar,
                                     sd=                sd,
                                     df=                df,
                                     n=                 n,
                                     alpha.right=       alpha.right,
                                     alpha.left=        alpha.left,
                                     stderr=            stderr,
                                     zc.right=          zc.right,
                                     xbarc.right=       xbarc.right,
                                     zc.left=           zc.left,
                                     xbarc.left=        xbarc.left,
                                     sided=             sided,
                                     xbar.left=         xbar.left,
                                     xbar.right=        xbar.right,
                                     xbar.otherside=    xbar.otherside,
                                     pvalue.left=       pvalue.left,
                                     pvalue.right=      pvalue.right,
                                     pvalue=            pvalue,
                                     power.left=        power.left,
                                     power.right=       power.right,
                                     power=             power,
                                     conf.left=         alpha.left, ## ifelse(sided != "right", 1 - alpha.right, alpha.right),
                                     conf.right=        alpha.right, ## ifelse(sided != "left", 1 - alpha.left, alpha.left),
                                     conf=              1 - (alpha.left + alpha.right)
                                     )
  attr(result,"table") <- result.table$normalTable
  attr(result,"prob") <- result.table$prob
  attr(result,"scales") <- result.table$scales

  ## attr(result,"call") <- deparse(match.call())  ## works for commandline call, not from shiny
  attr(result,"call") <- paste("NormalAndTplot(mean0=", ifelse(type=="hypothesis", mean0, NA),
                               ", mean1=",mean1,
                               ", xbar=", ifelse(type=="confidence", mean0, NA),
                               ", sd=",sd,
                               ", df=",df,
                               ", n=",n,
                               ", xlim=c(",xlim[1],",",xlim[2],")",
                               ", ylim=c(",ylim[1],",",ylim[2],")",
                               ", alpha.right=",alpha.right,
                               ", alpha.left=",alpha.left,
                               ", float=",float,
                               ", ntcolors=\"",ntcolors,"\"",
                               ", digits=",digits,
                               ", distribution.name=\"",distribution.name,"\"",
                               ", type=\"",type,"\"",
                               ", zaxis=",zaxis,
                               ", cex.z=",cex.z,
                               ", cex.prob=",cex.prob,
                               ##", main=",main,
                               ##", xlab=",xlab,
                               ##", ylab=",ylab,
                               ", prob.labels=",prob.labels,
                               ##", xhalf.multiplier=",xhalf.multiplier,
                               ")",
                               sep="")

  attr(result,"colors")=c(
    col.alpha            =col.alpha,
    col.notalpha         =col.notalpha,
    col.beta             =col.beta,
    col.power            =col.power,
    col.pvalue           =col.pvalue,
    col.pvaluetranslucent=col.pvaluetranslucent,
    col.critical         =col.critical,
    col.border           =col.border,
    col.text             =col.text,
    col.conf             =col.conf)

  class(result) <- c("NormalAndT", class(result))

  result
}

print.NormalAndT <- function(x, tablesOnPlot=TRUE, plot=TRUE,
                             scales=FALSE, prob=FALSE, call=FALSE,
                             ..., cex.table=.7, digits=4) {

  if (scales) {
    cat("\nscales\n")
    print(attr(x, "scales"))
  }
  if (prob) {
    cat("\nprobabilities\n")
    print(attr(x, "prob"))
  }
  if (call)
    cat("\ncall\n", attr(x, "call"), "\n")

  if (plot) {

    if (!tablesOnPlot) {
      return(NextMethod(x, "print"))
    }

    if (tablesOnPlot && !is.null(list(...)$position))
      stop("position= argument is incompatible with tablesOnPlot=TRUE")

    NextMethod(x, "print", position=c(0, .17, 1, 1))

    pushViewport(viewport(x = 0, y = 0,
                          width = .6,
                          height = .2,
                          just = c("left", "bottom")))

    gridExtra::grid.table(format(attr(x, "scales"), digits=digits),
                          parse=TRUE,
                          core.just="right", row.just="center", col.just="center",
                          gpar.rowtext = gpar(cex = cex.table, fontface = "bold"),
                          gpar.coltext = gpar(cex = cex.table, fontface = "bold"),
                          gpar.coretext = gpar(cex = cex.table))
    popViewport()

    pushViewport(viewport(x = .55, y = 0,
                          width = .4,
                          height = .2,
                          just = c("left", "bottom")))
    gridExtra::grid.table(format(round(attr(x, "prob"), digits=digits), nsmall=4),
                          parse=TRUE,
                          core.just="right", row.just="center", col.just="center",
                          gpar.rowtext = gpar(cex = cex.table, fontface = "bold"),
                          gpar.coltext = gpar(cex = cex.table, fontface = "bold"),
                          gpar.coretext = gpar(cex = cex.table))
    popViewport()
  }

  invisible(x)
}

