NormalAndT.power <- function(nt,
                             which=c("power","beta"),
                             digits=4, digits.top.axis=digits, digits.left=digits,
                             col.power=attr(nt, "color")["col.power"],
                             cex.top.axis=1, cex.left.axis=1,
                             lwd.reference=4, lwd.line=2,
                             main=which) {
  which <- match.arg(which)

  tables <- attr(nt, "table")
  ## scales <- attr(nt, "scales")
  ## prob <- attr(nt, "prob")

 ## nt.call <- as.list(nt$call[[2]])[-1]

  nt.type <- ifelse(is.na(tables["Confidence", "Probability"]), "hypothesis", "confidence")
  if (nt.type == "confidence")
    stop("power isn't meaningful for confidence graphs", call.=FALSE)

  nt.mean0 <- tables["bar(x)", "mu[0]"]
  nt.mean1 <- tables["bar(x)", "mu[1]"]
  nt.xbar  <- tables["bar(x)", "bar(x)"]
  nt.sd  <- tables["bar(x)", "sigma"]
##  nt.sd <- ifelse(is.null(nt.sd), 1, nt.sd)

  nt.n  <- tables["bar(x)", "n"]
##  nt.n <- ifelse(is.null(nt.n), 1, nt.n)

  nt.df  <- tables["bar(x)", "df"]
##  nt.df <- ifelse(is.null(nt.df), Inf, nt.df)

  nt.distribution <- ifelse(is.infinite(nt.df), "z", "t")

  nt.x <- seq(nt$x.limits[1], nt$x.limits[2], length=101)

  nt.crit <- attr(nt,"table")["bar(x)", c("bar(x)[crit.L]", "bar(x)[crit.R]")]
  nt.crit <- nt.crit[nt.crit != nt$x.limits]


  alphaNotMissing <- !is.na(tables["alpha", c("bar(x)[crit.L]", "bar(x)[crit.R]")])
  nt.sided <- if (sum(alphaNotMissing) == 2)
                "both"
              else
                ifelse(alphaNotMissing[1], "left", "right")

  switch(nt.distribution,
         normal=,
         z=pfunction <- function(q, mean, sd, lower.tail, df)
           pnorm(q=q, mean=mean, sd=sd, lower.tail=lower.tail),
         t=pfunction <- function(q, mean, sd, lower.tail, df)
           pt(q=(q-mean)/sd, df=df, lower.tail=lower.tail)
         )

  nt.ypower <- switch(nt.sided,
                      left=pfunction(
                        q=nt.crit,
                        df=nt.df,
                        mean=nt.x,
                        sd=nt.sd, lower.tail=TRUE),
                      right=pfunction(
                        q=nt.crit,
                        df=nt.df,
                        mean=nt.x,
                        sd=nt.sd, lower.tail=FALSE),
                      both=pfunction(
                        q=nt.crit[1],
                        df=nt.df,
                        mean=nt.x,
                        sd=nt.sd, lower.tail=TRUE) +
                          pfunction(
                            q=nt.crit[2],
                            df=nt.df,
                            mean=nt.x,
                            sd=nt.sd, lower.tail=FALSE)
                      )

  ## nt.alpha <- attr(nt,"table")["alpha",]
  nt.beta  <- attr(nt,"table")["beta","Probability"]
  nt.power <- attr(nt,"table")["power","Probability"]
  ## nt.p     <- attr(nt,"table")["p",]

  if (which == "power") {
    ylab=list(expression(atop(scriptstyle("power ="), 1-beta)), rot=0)
    nt.powerbeta <- nt.power
  }
  else {
    nt.ypower <- 1 - nt.ypower
    ylab=list(expression(beta), rot=0)
    nt.powerbeta <- nt.beta
  }

  xyplot(xlim=nt$x.limits, ylim=c(0,1), type="l", col="gray60", lwd=lwd.line,
         scales=list(y=list(at=seq(0,1,.2))),
         nt.ypower ~ nt.x, main=main, xlab=expression(mu[1]),
         ylab=ylab,
         par.settings=list(clip=list(panel=FALSE))) +
           layer({
             panel.abline(h=nt.powerbeta, v=nt.mean1,
                          lty=3, col=col.power, lwd=lwd.reference)
             panel.axis("left", at=nt.powerbeta,
                       labels=format(nt.powerbeta, digits=digits.left),
                        text.cex=cex.left.axis,
                        tck=6, outside=TRUE,
                        line.col=col.power, line.lwd=lwd.reference, line.lty=3)
             mean1.expr <- as.expression(substitute(mu1==mean1, c(alist(mu1=mu[1]),
                 list(mean1=format(nt.mean1, digits=digits.top.axis)))))
             panel.axis("top", at=nt.mean1,
                        labels=mean1.expr,
                        rot=0, outside=TRUE,
                        text.cex=cex.top.axis,
                        line.col=col.power, line.lwd=lwd.reference, line.lty=3)
           },
                 data=list(nt.mean1=nt.mean1, nt.powerbeta=nt.powerbeta,
                   col.power=col.power, digits.top.axis=digits.top.axis,
                   digits.left=digits.left, cex.top.axis=cex.top.axis,
                   cex.left.axis=cex.left.axis,
                   lwd.reference=lwd.reference))
}

NormalAndT.and.power <- function(nt, which="power",
                                   pnt=NormalAndT.power(
                                     nt,
                                     digits.top.axis=digits.top.axis,
                                     digits.left=digits.left,
                                     which=which,
                                     cex.top.axis=cex.top.axis, cex.left.axis=cex.left.axis,
                                     lwd.reference=4, lwd.line=2),
                                   digits=4, digits.top.axis=digits, digits.left=digits,
                                   cex.top.axis=1, cex.left.axis=1,
                                   display.ylab=TRUE) {

  pnt2 <- update(pnt, ylab=NULL, main=NULL)
  if (display.ylab)
    pnt2 <- pnt2 +
      layer(panel.axis(side="left", at=.5, labels=pnt$ylab[[1]],
                       text.cex=1.2,
                       tck=10, outside=TRUE, line.col="transparent"),
            data=list(pnt=pnt))
  ## update(pnt2, par.settings=list(layout.widths=list(left.padding=11)))


  nt2 <- nt + layer(panel.axis("bottom", at=mean(current.panel.limits()$xlim),
                               labels=nt$xlab, outside=TRUE, text.cex=1, rot=0,
                               tck=3, line.col="transparent"),
                    data=list(nt=nt))
  ## nt2

  pnt2.nt2 <- update(scales=list(y=list(rot=0)),
                     ylab=if (display.ylab)
                            list(c("", nt$ylab[[1]]), rot=0, cex=1.5)
                          else
                            NULL,
                     resizePanels(h=c(1,3),
                                  update(c(pnt2,
                                           nt2,
                                           x.same=FALSE,
                                           layout=c(1,2)),
                                         between=list(y=4),
                                         main=nt2$main
                                         )
                                  )
                     )
  pnt2.nt2$y.scales$at <- list(pnt2$y.scales$at, nt2$y.scales$at)
  class(pnt2.nt2) <- c("NormalAndT", class(pnt2.nt2))
  attributes(pnt2.nt2)[c("table", "prob", "scales", "call" )] <-
    attributes(nt2)[c("table", "prob", "scales", "call" )]
  pnt2.nt2

}

## nt <- NormalAndTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1))
## NormalAndT.and.power(nt)
## NormalAndT.and.power(nt, display.ylab=FALSE)
## update(NormalAndT.and.power(nt, display.ylab=FALSE), main=FALSE)
## update(NormalAndT.and.power(nt, display.ylab=FALSE), main=FALSE, par.settings=list(layout.widths=list(left.padding=7)))

## tt <- NormalAndTplot(mean0=2, mean1=4, sd=3, n=20, xlim=c(-.1, 6.1), df=4, distribution.name="t")
## NormalAndT.and.power(tt)

## ntc <- NormalAndTplot(xbar=2, sd=3, n=20, xlim=c(-.1, 4.1), type="confidence", alpha.left=.025, alpha.right=.025)
## ntc
## NormalAndT.and.power(ntc)


## vertical scale correct
## ttt <- NormalAndTplot(mean0=2, mean1=2.4, sd=.5, n=20, xlim=c(1.6, 2.7), df=4, distribution.name="t", prob.labels=FALSE)
## NormalAndT.and.power(ttt)
## NormalAndT.and.power(ttt, display.ylab=FALSE)
## update(NormalAndT.and.power(ttt, display.ylab=FALSE), main=FALSE, par.settings=list(layout.widths=list(left.padding=7)))


## ## from inside
##                                  both <- c(pnt2,
##                                            nt2,
##                                            x.same=FALSE,
##                                            layout=c(1,2))
## both
## both$y.scales$at <- list(pnt2$y.scales$at, nt2$y.scales$at)
## both
## pnt2.nt2$y.scales$at <- list(pnt2$y.scales$at, nt2$y.scales$at)
## pnt2.nt2
