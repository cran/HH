## Mystery time series X
X <- as.rts(scan(hh("datasets/tser.mystery.X.dat")))

X.dataplot <- tsacfplots(X, lwd=1, pch.seq=16, cex=.7)
## trellis.device(file=hh("tser/figure/arima.sim.X.eps"), postscript, horizontal=TRUE); strip.background0()
X.dataplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.X.eps"))

X.loop <- if.R(s=
               arma.loop(X, model=list(order=c(2,0,2)))
               ,r=
               arma.loop(X, order=c(2,0,2))
               )
X.diag <- rearrange.diag.arma.loop(diag.arma.loop(X.loop, X))
X.diagplot <- tsdiagplot(armas=X.loop, ts.diag=X.diag, lwd=1)
## trellis.device(file=hh("tser/figure/arima.sim.Xd.eps"), postscript, horizontal=TRUE); strip.background0()
X.diagplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.Xd.eps"))
X.loop
X.loop[["1","1"]]



## Mystery time series Y
Y <- as.rts(scan(hh("datasets/tser.mystery.Y.dat")))

Y.dataplot <- tsacfplots(Y, lwd=1, pch.seq=16, cex=.7)
## trellis.device(file=hh("tser/figure/arima.sim.Y.eps"), postscript, horizontal=TRUE); strip.background0()
Y.dataplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.Y.eps"))

Y.loop <- if.R(s=
               arma.loop(Y, model=list(order=c(2,0,2)))
               ,r=
               arma.loop(Y, order=c(2,0,2))
               )
Y.diag <- rearrange.diag.arma.loop(diag.arma.loop(Y.loop, Y))
Y.diagplot <- tsdiagplot(armas=Y.loop, ts.diag=Y.diag, lwd=1)
## trellis.device(file=hh("tser/figure/arima.sim.Yd.eps"), postscript, horizontal=TRUE); strip.background0()
Y.diagplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.Yd.eps"))
Y.loop
Y.loop[["1","1"]]



## Mystery time series Z
Z <- as.rts(scan(hh("datasets/tser.mystery.Z.dat")))

Z.dataplot <- tsacfplots(Z, lwd=1, pch.seq=16, cex=.7)
## trellis.device(file=hh("tser/figure/arima.sim.Z.eps"), postscript, horizontal=TRUE); strip.background0()
Z.dataplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.Z.eps"))

Z.loop <- if.R(s=
               arma.loop(Z, model=list(order=c(2,0,2)))
               ,r=
               arma.loop(Z, order=c(2,0,2))
               )
Z.diag <- rearrange.diag.arma.loop(diag.arma.loop(Z.loop, Z))
Z.diagplot <- tsdiagplot(armas=Z.loop, ts.diag=Z.diag, lwd=1)
## trellis.device(file=hh("tser/figure/arima.sim.Zd.eps"), postscript, horizontal=TRUE); strip.background0()
Z.diagplot
## dev.off()
## export.eps(hh("tser/figure/arima.sim.Zd.eps"))
Z.loop
Z.loop[["1","1"]]
