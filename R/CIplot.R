## confintervaldata

## confinterval.matrix

## confintervalplot

## CIplot

## library(lattice)

confintervaldata <- function(n.intervals=100,
                             n.per.row=40,
                             pop.mean=0,
                             pop.sd=1,
                             conf.level=.95,
                             seed, ...) {
  if (!missing(seed)) set.seed(seed)
  result <-
  matrix(rnorm(n.intervals * n.per.row, mean=pop.mean, sd=pop.sd),
         nrow=n.intervals,
         ncol=n.per.row)
  attr(result, "pop.mean") <- pop.mean
  attr(result, "pop.sd") <- pop.sd
  attr(result, "conf.level") <- conf.level
  attr(result, "n.per.row") <- n.per.row
  attr(result, "conf.level") <- conf.level
  if (!missing(seed)) attr(result, "seed") <- seed
  result
}

confinterval.matrix <- function(x, conf.level=attr(x, "conf.level"), ...) {
  x.test <- apply(x, 1, t.test, conf.level=conf.level)
  x.ci <- data.frame(t(sapply(x.test, `[[`, "conf.int")))
  x.ci$center <- sapply(x.test, `[[`, "estimate")
  names(x.ci) <- c("lower","upper","center")
  attr(x.ci, "pop.mean")   <- attr(x, "pop.mean")
  attr(x.ci, "pop.sd")     <- attr(x, "pop.sd")
  attr(x.ci, "conf.level") <- conf.level
  attr(x.ci, "n.per.row")  <- attr(x, "n.per.row")
  attr(x.ci, "seed")       <- attr(x, "seed")
  x.ci
}


confintervalplot <- function(x.ci,
                             n.intervals=nrow(x.ci),
                             pop.mean=attr(x.ci, "pop.mean"),
                             pop.sd=attr(x.ci, "pop.sd"),
                             n.per.row=attr(x.ci, "n.per.row"),
                             xlim,
                             ylim,
                             ...)
  {
  conf.level <- 100 * attr(x.ci, "conf.level")
  inside <- (x.ci$lower <= pop.mean) & (x.ci$upper >= pop.mean)
  conf.percent <- 100 * sum(inside) / n.intervals
  alph2 <- (1-conf.level/100)/2
  df <- n.per.row - 1
  trellis.colors <- trellis.par.get()$superpose.symbol$col
  xyplot((1:n.intervals) ~ lower + upper + center, data=x.ci, outer=FALSE,
         panel=function(...) {
           panel.abline(v=pop.mean)
           panel.segments(x1=x.ci$lower, y1=(1:n.intervals),
                          x2=x.ci$upper, y2=(1:n.intervals),
                          lty=inside+1,
                          col=c("red","black")[inside+1])
           panel.axis("right", at=which(!inside),
                      line.col="red", text.col="red",
                      outside=TRUE)
           panel.xyplot(...,
                        col=c(trellis.colors[1:2], "black"),
                        cex=c(.8, .8, 1.2),  pch=c(19,19,43))
         },
         par.settings=list(
           superpose.symbol=list(pch=19),
           clip=list(panel=FALSE)),
         key=list(border=TRUE,
           title="Confidence Interval",
           cex.title=1.1,
           text=list(c("Limits","True Value")),
           text=list(c("lower", "included")),
           points=list(pch=19, col=c(trellis.colors[1],0)),
           lines=list(lty=c(0,2), col=c("transparent","black")),
           text=list(c("center", "excluded")),
           points=list(pch=43, col=c(1,0), cex=1.5),
           lines=list(lty=c(0,1), col=c("transparent","red")),
           text=list(c("upper","")),
           points=list(pch=19, col=c(trellis.colors[2],0))),
         xlab=bquote(.(conf.percent) * "% of the " * .(n.intervals) *
                       " observed CI lines include the True Value " *
                         mu == .(pop.mean)),
         main=bquote("calculated " * .(conf.level) *
                       "% Confidence Intervals using " *
                         italic(t)[ .(df) * ", " * .(alph2) ]),
         sub=bquote("from " * .(n.intervals) * " independent sets of n=" * .(n.per.row) *
                      " observations from N(" *
                        mu * "=" * .(pop.mean) * ", " * sigma * "=" *
                          .(pop.sd) * ")"),
         ylab=NULL,
         xlim=xlim,
         ylim=ylim)
}


CIplot <- function(n.intervals=100,
                         n.per.row=40,
                         pop.mean=0,
                         pop.sd=1,
                         conf.level=.95, ...) {
  x <- confintervaldata(n.intervals=n.intervals,
                   n.per.row=n.per.row,
                   pop.mean=pop.mean,
                   pop.sd=pop.sd,
                   conf.level=conf.level, ...) ## seed
  x.ci <- confinterval.matrix(x)
  confintervalplot(x.ci, ...) ## xlim, ylim
}

## if (FALSE) {

## tmp.data <- confintervaldata()
## tmp.ci <- confinterval.matrix(tmp.data)
## confintervalplot(tmp.ci)

## CIplot()
## }
