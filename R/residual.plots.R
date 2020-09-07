residual.plots <- function(lm.object, X=dft$x, layout=c(dim(X)[2],1),
                           par.strip.text=list(cex=.8),
                           scales.cex=.6,
                           na.action=na.pass,
                           y.relation="free",
                           ...) {
  lm.formula <- as.formula(lm.object)
  lm.data <- try(eval(lm.object$call$data), silent=TRUE)
  if (class(lm.data) == "Error" || class(lm.data)=="try-error") ## S-Plus || R
    {
      lm.data <- lm.object$x
      if.R(r={
        data.frameAux.default <- NA ## make R-2.6.0dev happy
      },
           s=if(inherits(lm.data, "model.matrix"))  ## This is needed by S-Plus
           lm.data <- data.frame(data.frameAux.default(lm.data),
                                 check.names=FALSE)
           )
      lm.data.y <- as.numeric(lm.object$y)
      if (is.null(lm.data) || is.null(lm.data.y))
        stop("Please recompute the 'lm.object' with 'x=TRUE, y=TRUE'.")
      lm.data <- cbind(lm.data.y, lm.data)
      names(lm.data)[1] <- as.character(lm.formula[[2]])
    }
  
  dft <- do.formula.trellis.xysplom(lm.formula, lm.data, na.action)

  resids <- resid(lm.object)
  yhat <- predict(lm.object, type="terms")

  if.R(r={
    X <- data.frame(X, check.names=FALSE)
  },
       s={
         if (inherits(X, "model.matrix"))
           X <- data.frame(data.frameAux.default(X), check.names=FALSE)
         ##
         ## Don't ask, just use it.  But if you want to know: as.data.frame()
         ## applied to a model.matrix produces a list with the model.matrix
         ## as its sole component.  S-Plus does this intentionally with the
         ## function data.frameAux.model.matrix.  They do not consider it a
         ## bug.  The statement here creates an ordinary data.frame in which
         ## each column consists of one column from the model.matrix.
       })
  
  if (dim(yhat)[[2]] != dim(X)[[2]])
   stop("The model has factors or interactions.  Please use the `X=' argument.")
  partial.resids <- yhat + resids
  dimnames(partial.resids)[[2]] <- rep("part.res|X", ncol(partial.resids))

  ## data plots
  ## y against X
  y.X <-
  xysplom(y=dft$y, x=X,
          main=paste(names(dft$y), "x variables", sep=" ~ "),
          xlab="", ylab="",
          ## ylab=names(dft$y),
          ## xlab="x variables",
          abline=TRUE,
          par.strip.text=par.strip.text,
          between=list(x=1,y=1),
          layout=layout,
          scales=list(
            cex=scales.cex,
            x=list(relation="free"),
            y=list(relation=y.relation),
            alternating=FALSE),
          ...)


  ## residual plots
  ## residuals against X
  res.X <-
    xysplom(y=data.frame(residuals=resids), x=X,
            main="residuals ~ x variables",
            xlab="", ylab="",
            ## ylab="residuals",
            ## xlab="x variables",
            abline=TRUE,
            par.strip.text=par.strip.text,
            between=list(x=1,y=1),
            layout=layout,
            scales=list(
              cex=scales.cex,
              x=list(relation="free"),
              y=list(relation=y.relation),
              alternating=FALSE),
            ...)


  ## partial residuals plots
  ## partial residuals against X
  pres.X <-
    xysplom(y=partial.resids,
            x=X,
            cartesian=FALSE,
            main="partial residuals of y against the other X columns ~ x variables",
            xlab="", ylab="",
            ## ylab="partial residuals of y against the other X columns",
            ## xlab="x variables",
            beta=TRUE,
            par.strip.text=par.strip.text,
            between=list(x=1,y=1),
            layout=layout,
            scales=list(
              cex=scales.cex,
              x=list(relation="free"),
              y=list(relation=y.relation),
              alternating=FALSE),
            pch=16,
            ...)
  

  ## added variable plots
  ## partial residuals against X.j
  X.res <- X.residuals(lm.object)
  names(X.res) <- paste(names(X.res), "X", sep="|")

  firstColumn <- function(x) {
    llx <- length(levels(x))
    if (llx==0) TRUE else c(TRUE, rep(FALSE, max(llx-2, 0)))
  }
  X.resSubscript <- X.res[ , unlist(sapply(X, firstColumn))]

  main4 <- if (length(X.resSubscript) == length(X.res))
    "partial residuals of y against the other X columns ~ residuals of x against the other X columns"
  else
     "partial residuals of y against the other X columns ~ residuals of x against the other X columns\nOnly the first dummy variable is shown for factors"
  pres.Xj <-
    xysplom(y=partial.resids,
            x=X.resSubscript,
            cartesian=FALSE,
            main=main4,
            xlab="", ylab="",
            ## ylab="partial residuals of y against the other X columns",
            ## xlab="residuals of x.j \n against other x variables",
            beta=TRUE,
            par.strip.text=par.strip.text,
            between=list(x=1,y=1),
            layout=layout,
            scales=list(
              cex=scales.cex,
              x=list(relation="free"),
              y=list(relation=y.relation),
              alternating=FALSE),
            pch=16,
            ...)
  
  
  list(y.X=y.X, res.X=res.X, pres.X=pres.X, pres.Xj=pres.Xj)
}
## source("c:/HOME/rmh/HH-R.package/HH/R/residual.plots.s")
## assignInNamespace("residual.plots", residual.plots, "HH")


## ## longley regression example.
## ## data is included with S-Plus
## 
## longley <- data.frame(longley.x, Employed = longley.y)
## longley.lm <- lm( Employed ~ . , data=longley)
## residual.plots(longley.lm)
##
##
#### sample change of layout
##
## tmp <- residual.plots(longley.lm)
## for (i in seq(along=tmp))
##   tmp[[i]]$layout <- c(1,6)
## print(position=c(-.025,0, .275,1.6), more=TRUE, tmp[[1]])
## print(position=c( .225,0, .525,1.6), more=TRUE, tmp[[2]])
## print(position=c( .475,0, .775,1.6), more=TRUE, tmp[[3]])
## print(position=c( .725,0,1.025,1.6), more=FALSE, tmp[[4]])
##
## export.eps(hh("regb/figure/longley.resid.eps"))
