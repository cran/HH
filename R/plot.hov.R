"plot.hov" <-
function(x, data=sys.parent(), method="bf",
                     transpose.in=TRUE, ...) {
  if (method != "bf")
    stop("Only 'bf', Brown-Forsyth method is currently available.")
  if.R(r={
    lPF <- latticeParseFormula(x, data=data)
    y <- lPF$left
    group <- lPF$right
    y.name <- lPF$left.name
    group.name <- lPF$right.name
  }, s={
    dft <- do.formula.trellis(x)
    y          <-    eval(dft$expr[[1]], local=data)
    group      <-    eval(dft$expr[[2]], local=data)
    y.name     <- deparse(dft$expr[[1]])
    group.name <- deparse(dft$expr[[2]])
  })
  plot.hov.bf(y, group, y.name, group.name, transpose=transpose.in)
}

"plot.hov.bf" <-
function(x, group,
                        y.name=deparse(substitute(x)),
                        group.name=deparse(substitute(group)),
                        transpose.in=TRUE, ...) {
  med <- tapply(x, group, median)
  w <- x - med[group]
  z <- abs(w)
  yy.names <- c("y", "y-med(y)", "abs(y-med(y))")
  n <- length(x)
  hov.dat <- data.frame(yy=c(x, w, z),
                        group=rep(group,3),
                        which=rep(ordered(yy.names, levels=yy.names), c(n,n,n)))

  if.R(r=
       bwplot(yy ~ group | which, data=hov.dat,
              panel="panel.hov",
              par.strip.text=list(cex=1.4),
              scales=list(
                y=list(cex=1, relation="sliced"),
                x=list(cex=1)),
              xlab=list(group.name, cex=1.4),
              ylab=list(y.name, cex=1.4),
              layout=c(3,1))
       ,s={tmp <- bwplot(group ~ yy | which, data=hov.dat,
                         panel="panel.hov",
                         par.strip.text=list(cex=1.4),
                         scales=list(
                           x=list(cex=1, relation="sliced"),
                           y=list(cex=1)),
                         xlab=list(y.name, cex=1.4),
                         ylab=list(group.name, cex=1.4),
                         layout=c(3,1))
           if (transpose.in) tmp <- t(tmp)
           tmp
       })
}

"panel.hov" <-
  function(..., transpose=TRUE) {
    if.R(r={
      panel.bwplot(...)
      panel.abline(h=0)
    },s={
      panel.bwplott(..., transpose=transpose)
      if (get("cell", frame=sys.parent()) != 1)
        if (transpose)
          panel.abline(h=0)
        else
          panel.abline(v=0)
    })
  }

