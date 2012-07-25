library(HH)

#### iinf/code/vocab.s
data(vocab)
stem(vocab$score)
t.test(vocab$score, mu=10)


#### iinf/code/teachers.s
data(teachers)
teachers[c(1,2,32),]
if.R(s=
     t(bwplot(teachers))
     ,r={
       tmp <- data.frame(x=unlist(teachers),
                         language=rep(names(teachers), c(32,32)))
       bwplot(x ~ language, data=tmp)
   }
)
if.R(r=parallelplot( ~ teachers),
     s=parallel( ~ teachers))

teachers.diff <- teachers$English - teachers$Greek
teachers.diff
if.R(r=
     parallelplot( ~ teachers,
                  lty=c(1,3)[1+(teachers.diff > 0)],  ## control line type
                  scales=list(cex=1.5))               ## control axis labels
     , s=
     parallel( ~ teachers,
              lty=c(1,3)[1+(teachers.diff > 0)],  ## control line type
              scales=list(cex=1.5))               ## control axis labels
     )
stem(teachers.diff)                  ## first try
if.R(s=
     stem(teachers.diff, nl=5, scale=-1)  ## better scaling
     ,r=
     stem(teachers.diff, scale=2)  ## different scaling
     )
stem(sqrt(teachers.diff + 17))
t.test(sqrt(teachers.diff + 17), mu=sqrt(17))
t.test(teachers.diff)


#### iinf/code/teachers2.s
## teachers is defined in teachers.s (above)

tmp <- data.frame(errors=as.vector(unlist(teachers)),
                  sent.lang=rep(names(teachers), c(32,32)),
                  sentence=rep(1:32,2))
tmp[c(1,2,32,33,34,64),]


new.order <- order(sign(teachers$English - teachers$Greek),
                   apply(teachers, 1, min))
if.R(s=
     ordered(tmp$sentence) <- new.order
     ,r=
     tmp$sentence <- ordered(tmp$sentence, levels=new.order)
     )

tpg <- trellis.par.get("superpose.symbol")
tpg$pch[3] <- 32 ## ascii for blank

tmp.trellis <-
dotplot(sentence ~ errors, data=tmp,
        panel=function(x,y,...) {
          superpose.symbol <- trellis.par.get("superpose.symbol")
          dot.line <- trellis.par.get("dot.line")
          panel.abline(h = y, lwd = dot.line$lwd, lty = dot.line$lty,
                 col = dot.line$col)
          panel.superpose(x,y,...)
          panel.abline(h=21.5, lty=2)
          if.R(r={
            panel.segments(-12,21.5,44,21.5, lty=3, xpd=TRUE)
          },
               s={
                 segments(-12,21.5,44,21.5, lty=3, xpd=TRUE)
                 mtext(side=2, at=c(28.5,13.5), line=6,
                       c("Greek\nsmaller\nerror\ncount",
                         "English\nsmaller\nerror\ncount"))
               })
        },
        groups=sent.lang,
        key=list(text=
          list(c("",
            "English for", "Greek Speakers","",
            "Greek for", "English Speakers","")),
          points=Rows(tpg, c(3,1,3,3,2,3,3)),
          space="right",
          border=TRUE),
        ylab=if.R(
          r=list(
            rev(c("Greek\nsmaller\nerror\ncount",
                  "English\nsmaller\nerror\ncount")),
            rot=0),
          s=NULL)
)
print(position=c(.15,0,.85,1),
      tmp.trellis
      )
## export.eps(hh("iinf/figure/teachers-dot.eps"))

## based on trellis library example.overplot


#### iinf/code/ex.qqnorm.s

x1 <- rnorm(100,0,1)
x3 <- runif(100,-3,3)
x2 <- x1/x3
x4 <- x1*exp(-abs(x1))
x5 <- x1^2
x6 <- -x5

dist.names <- c("normal",
                "heavy-tailed",
                "uniform",
                "thin-tailed",
                "positively skewed",
                "negatively skewed")

qqnorm.dat <-
data.frame(dist=factor(rep(dist.names, rep(100,6)), levels=dist.names),
           x=c(sort(x1), sort(x2), sort(x3), sort(x4), sort(x5), sort(x6)),
           qn=rep(qnorm(ppoints(length(x1))),6)
)

xyplot(x ~ qn | dist, data=qqnorm.dat,
       par.strip.text=list(cex=1.5),
       layout=c(2,3), as.table=TRUE,
       scales=list(cex=1, y=list(relation="free"), alternating=FALSE),
       between=list(x=1,y=1),
       xlab="Quantiles of Standard Normal",
       ylab="distribution")
## export.eps(hh("iinf/figure/iinf.f.qqnorm.ps"))


#### iinf/code/ex.tquant.s
old.par <- par(cex=1.2, pch=16, pty="s",
               mar=par("mar")+if.R(s=c(0,1,4,0), r=c(0,7,4,3)))
par(plt=par()$plt+c(-.21,-.21,0,0))

## d1 <- rt(100,5)
## the d1 we used is in file ex.tquant.d1.s
source(hh.old("scripts/ex.tquant.d1.s")) ## dump of the d1 object
## ex.tquant.d1.s  lives in the same script directory as Ch05-iinf.r
## source("c:/HOME/rmh/HH-R.package/HH/inst/scripts/ex.tquant.d1.s")

q3 <- qt(ppoints(d1), 3)
q5 <- qt(ppoints(d1), 5)
q7 <- qt(ppoints(d1), 7)
qn <- qnorm(ppoints(d1))


qqplot(d1, q3,
       type="l", lty=1,
       main="Selected Q-Q Plots",
       xlab="sample from t with 5 df",
       ylab="comparison values",
       xlim=c(-6,6), ylim=c(-6,6))

lines(sort(d1), q5, lty=2)

lines(sort(d1), q7, lty=3)

lines(sort(d1), qn, lty=4)

abline(a=0, b=1, lty=5)

if.R(r=
     old.xpd <- par(xpd=TRUE)
     ,s=
     {}
   )
legend(7, 2,
       c("quantiles of t, 3 df",
         "quantiles of t, 5 df",
         "quantiles of t, 7 df",
         "quantiles of normal",
         "slope=1"),
       lty=c(1,2,3,4,5))
if.R(r=
     par(old.xpd)
     ,s=
     {}
     )
par(old.par)
## export.eps(hh("iinf/figure/iinf.f1.ps"))


#### iinf/code/ex.tquant.d1.s
## read above


#### iinf/code/ex.ttail.s
## continues with data from ex.tquant.s
ppd <- ppoints(d1)
    d3 <- dt(q3, 3)
    d5 <- dt(q5, 5)
    d7 <- dt(q7, 7)
    dn <- dnorm(qn)

    plot( x=q3, y=d3, lty=1, type="l", ylim=c(0,.4),
         main="density functions",
         xlab="quantile q", ylab="density")
    lines(x=q5, y=d5, lty=2)
    lines(x=q7, y=d7, lty=3)
    lines(x=qn, y=dn, lty=4)

    legend(-5.5, .3,
           c("t, 3 df",
             "t, 5 df",
             "t, 7 df",
             "normal"),
           lty=c(1,2,3,4))

    abline(h=0)
## export.eps(hh("iinf/figure/iinf.f2.ps"))


#### iinf/code/goffit.s
par(mfcol=c(2,3))
old.par <- par(mar=par("mar")+c(0,1,0,0))

## t9 <- rt(100,9)
## the t9 we used is in file goffit-t9-t3-t5.s
source(hh.old("scripts/goffit-t9-t3-t5.s")) ## dump of the t9 object
## goffit-t9-t3-t5.s lives in the same script directory as Ch05-iinf.r
## source("c:/HOME/rmh/HH-R.package/HH/inst/scripts/goffit-t9-t3-t5.s")
## to reproduce the data in hh("iinf/figure/iinf.f3.ps.gz")

t9s <- sort(t9)
p.fractions <- ppoints(t9s)
length(p.fractions)
p.fractions[1:5]
p.fractions[96:100]



## one sample when H0 is true
if.R(s=
     ks.gof(t9, distribution="t", df=9)
     ,r=
     ks.test(t9, y="pt", df=9)
     )
t9.quantiles <- qt(p.fractions, 9)

plot(x=range(t9, t9.quantiles), y=c(0,1), type="n", cex=1)
title(main="KS for H0: t9 ~ t with 9 df", cex=.9)
lines(x=t9.quantiles, y=p.fractions)    #reference distribution
points(x=t9s, y=p.fractions, pch=16, cex=.3)

vert.obs.99 <- pt(t9s, 9)               #additional reference distribution
lines(x=t9s, y=vert.obs.99, lty=2)      #to be used for finding ks statistic

if.R(s=
     segments(x1=t9s, y1=p.fractions, x2=t9s, y2=vert.obs.99)
     ,r=
     segments(x0=t9s, y0=p.fractions, x1=t9s, y1=vert.obs.99)
     )
max(abs(vert.obs.99-p.fractions))       #approx ks statistic

plot(y=p.fractions-vert.obs.99, x=t9s, type="h", ylim=c(-.15,.15), cex=1)
title(main="deviations: H0 is true", cex=.9)



## one sample when H0 is false

if.R(s=
     ks.gof(t9, distribution="t", df=2)
     ,r=
     ks.test(t9, y="pt", df=2)
     )
t2.quantiles <- qt(p.fractions, 2)

plot(x=range(t9, t2.quantiles), y=c(0,1), type="n", cex=1)
title(main="KS for H0: t9 ~ t with 2 df", cex=.9)
lines(x=t2.quantiles, y=p.fractions)    #reference distribution
points(x=t9s, y=p.fractions, pch=16, cex=.3)

vert.obs.92 <- pt(t9s, 2)               #additional reference distribution
lines(x=t9s, y=vert.obs.92, lty=2)      #to be used for finding ks statistic

if.R(s=
     segments(x1=t9s, y1=p.fractions, x2=t9s, y2=vert.obs.92)
     ,r=
     segments(x0=t9s, y0=p.fractions, x1=t9s, y1=vert.obs.92)
     )
max(abs(vert.obs.92-p.fractions))       #approx ks statistic

plot(y=p.fractions-vert.obs.92, x=t9s, type="h", ylim=c(-.15,.15), cex=1)
title(main="deviations: H0 is false", cex=.9)


## two sample

## t3 <- rt(100,3)
## t5 <- rt(100,5)
if.R(s=
     ks.gof(t3, t5)
     ,r=
     ks.test(t3, t5)
     )

t3s <- sort(t3)
t5s <- sort(t5)

tpg <- trellis.par.get("superpose.line")

matplot(x=cbind(t3s, t5s), y=(1:100)/101,
        type="n",  xaxt="n", yaxt="n", xlab="", ylab="")
matpoints(x=cbind(t3s, t5s), y=(1:100)/101,
        pch=c(16,17), cex=.3, col=tpg$col)
title(main="two-sample KS", cex=.9)
title(xlab="t3 and t5",
      ylab="empirical distribution",
      cex=1)
axis(1, cex=1)
axis(2, cex=1)

matplot(x=cbind(t3s, t5s), y=(1:100)/101,
        type="n",  xaxt="n", yaxt="n", xlab="", ylab="")
matlines(x=cbind(t3s, t5s), y=(1:100)/101,
         lty=c(1,6), col=tpg$col)
title(main="two-sample KS", cex=.9)
title(xlab="t3 and t5",
      ylab="empirical distribution",
      cex=1)
axis(1, cex=1)
axis(2, cex=1)

par(old.par)
par(mfcol=c(1,1))
## export.eps(hh("iinf/figure/iinf.f3.ps"))


#### iinf/code/goffit-t9-t3-t5.s
## used by iinf/code/goffit.s, and not directly by user.
