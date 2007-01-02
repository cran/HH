tpg.col <- function(col.in=1) {
  tpg <- if.R(s=attr(get(".Device", where=0), "trellis.settings"),
              r=trellis.par.get())
  old.tpg <- tpg
  
  tpg$axis.line$col <- col.in
  trellis.par.set("axis.line", tpg$axis.line)
  
  tpg$add.text$col <- col.in
  trellis.par.set("add.text", tpg$add.text)
  
  tpg$plot.line$col <- col.in
  trellis.par.set("plot.line", tpg$plot.line)
  
  tpg$plot.symbol$col <- col.in
  trellis.par.set("plot.symbol", tpg$plot.symbol)
}


mmc.jcgs.explain <-
  function(group, n, ybar, ms.5, crit.point=1.96, ylabel="ylabel",
           method="user-defined",
           xlim,
           ylim=xlim,
           exit=F,
           col.in=c(1,1,1),
           cex.lab=1,
           cex.tick=1,
           par.settings=NULL) {
    tpg.col(col.in[1])
    par(col=col.in[1])
    ybar.mat <- matrix(ybar, length(ybar), length(ybar))
    ry <- range(ybar)
    if (missing(xlim)) xlim <- ry + c(-1,1)*.7*diff(ry)
    xyplot.args <-
      list(as.vector(ybar.mat) ~ as.vector(t(ybar.mat)),
           aspect=1, xlim=xlim, ylim=ylim,
           xlab=list("h=ybar", col=col.in[1], cex=cex.lab),
           ylab=list("v=ybar", col=col.in[1], cex=cex.lab),
           crit.point=crit.point,
           uy=ybar,
           uy.labels=group,
           ms.5=ms.5,
           exit=exit,
           col.in=col.in,
           cex.lab=cex.lab,
           cex.tick=cex.tick,
           scales=list(col=col.in[1], cex=cex.tick),
           panel=function(x, y, ...,
             crit.point, uy, uy.labels, ms.5, exit, col.in,
             cex.lab=1, cex.tick=1) {

             if.R(r={
               segments <- panel.segments
               text <- panel.text
             },
                  s={})
             
             ## (ybar, ybar) points
             tpg.col(col.in[1])
             panel.xyplot(x, y, ...,
                          col=col.in[1])
             ## square in (h,v) and (d,m) coordinates
             for (i in uy) {
               segments(min(uy), i, max(uy), i, lty=2, col=col.in[1])
               segments(i, min(uy), i, max(uy), lty=2,
                        col=col.in[1])
             }
             ## labels for constant v and h lines
             text(x=uy, y=rep(min(y)-.5, 4), uy.labels,
                  col=col.in[1], cex=cex.tick)
             text(y=uy, x=rep(min(y)-.5, 4), uy.labels,
                  col=col.in[1], cex=cex.tick)

             ## means on m axis
             panel.abline(a=0, b=1, col=col.in[1])
             text(x=max(y)+c(1.2,.5), y=max(y)+c(.4,1.6),
                  c("d = h-v = 0","(m-axis)"),
                  srt=45, adj=0,
                  col=col.in[1],
                  cex=cex.lab+.1)
             ## ticks on m axis
             segments(uy-.025*diff(range(y)),
                      uy+.025*diff(range(y)),
                      uy+.025*diff(range(y)),
                      uy-.025*diff(range(y)),
                      col=col.in[1])           

             ## turn off clipping in R
             ## --- needed for axis ticks and labels outside the plot region
             if.R(r={
               cpl <- current.panel.limits()
               pushViewport(viewport(xscale = cpl$xlim,
                                     yscale = cpl$ylim,
                                     clip = "off"))
               ## put anything you want unclipped inside this:
             },s={})

             ## (0,0) point and axes
             segments(44.5,
                      44.25,
                      44.5,
                      47,
                      xpd=T,
                      col=col.in[1])
             segments(44.25,
                      44.5,
                      47,
                      44.5,
                      xpd=T,
                      col=col.in[1])
             segments(44.5,
                      44.5,
                      45.5,
                      45.5,
                      xpd=T,
                      col=col.in[1])
             segments(45.5,
                      45.5,
                      par("usr")[1],
                      par("usr")[1],
                      xpd=T,
                      lty=2,
                      col=col.in[1])
             segments(40.5,
                      48.5,
                      46.5,
                      42.5,
                      xpd=T,
                      col=col.in[1])
             text(x=c(42.8,42.2), y=c(47.2,46.0),
                  c("(d-axis)","m = (h+v)/2 = 0"), srt=-45,
                  col=col.in[1],
                  cex=cex.lab+.1)

             ## restore clipping in R
             if.R(r=
                  ## end of unclipped part
                  upViewport()
                  ,s={})


             if (exit==1) return()
             ## export.eps(hh("presentations/jcgs/figure/mmc1-a.eps"))


             tpg.col(col.in[2])
             ## ticks in m coordinates
             panel.abline(a=8, b=1,
                          col=col.in[2])
             segments(uy-8/2,
                      uy+8/2,
                      uy-8/2+.05*diff(range(y)),
                      uy+8/2-.05*diff(range(y)),
                      col=col.in[2])           
             ## m labels, ybar
             text(x=uy-8/2-.4,
                  y=uy+8/2+.4,
                  round(uy,1),
                  col=col.in[2], cex=cex.tick)
             text(x=uy[1]-8/2-.4+.8,
                  y=uy[1]+8/2+.4+.8,
                  "",
                  col=col.in[2], cex=cex.tick)
             ## perpendiculars from A to m-axis
             segments(uy[1],uy[1], uy[1]-8/2, uy[1]+8/2, lty=3,
                      col=col.in[2]) ## to m-axis

             if (exit==4) return()
             ## export.eps(hh("presentations/jcgs/figure/mmc1-b0.eps"))

             
             ## differences on d axis
             panel.abline(a=2*min(y)-.2*diff(range(y))-.8, b=-1,
                          col=col.in[2])
             segments(uy-(uy-min(uy))/2-.1*diff(range(y))-.4,
                      min(uy)-(uy-min(uy))/2-.1*diff(range(y))-.4,
                      uy-(uy-min(uy))/2-.05*diff(range(y))-.4,
                      min(uy)-(uy-min(uy))/2-.05*diff(range(y))-.4,
                      col=col.in[2])
             segments( (0:6)/2+min(uy)-.10*diff(range(y))-.4,
                      -(0:6)/2+min(uy)-.10*diff(range(y))-.4,
                      (0:6)/2+min(uy)-.15*diff(range(y))-.4,
                      -(0:6)/2+min(uy)-.15*diff(range(y))-.4,
                      col=col.in[2])
             text(x= (0:7)/2+min(uy)-.18*diff(range(y))-.4,
                  y=-(0:7)/2+min(uy)-.18*diff(range(y))-.4,
                  c(0,"",2,"",4,"",6, ""),
                  col=col.in[2], cex=cex.tick)
             ## perpendicular from A to d-axis
             segments(uy[1],uy[4], uy[1]-3.8, uy[4]-3.8, lty=3,
                      col=col.in[2]) ## to d-axis
             if (exit==2) return()
             ## export.eps(hh("presentations/jcgs/figure/mmc1-b.eps"))

             tpg.col(col.in[3])
             ## CI for ybar2-ybar4
             segments(uy[2]-ms.5*sqrt(1/n[2]+1/n[4])*crit.point/2,
                      uy[4]+ms.5*sqrt(1/n[2]+1/n[4])*crit.point/2,
                      uy[2]+ms.5*sqrt(1/n[2]+1/n[4])*crit.point/2,
                      uy[4]-ms.5*sqrt(1/n[2]+1/n[4])*crit.point/2,
                      col=col.in[3])
           }
           )
    if (!is.null(par.settings))
      xyplot.args$par.settings <- par.settings
    do.call("xyplot", xyplot.args)
  }
