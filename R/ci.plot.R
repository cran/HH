"ci.plot" <-
function(lm.object, ...)
  UseMethod("ci.plot")

"ci.plot.lm" <-
function(lm.object,
         xlim=range(data[, x.name]),
         newdata,
         conf.level=.95,
         data=model.frame(lm.object),
         newfit,
         ylim,
         pch=16,
         main.cex=1,
         main=list(paste(100*conf.level,
           "% confidence and prediction intervals for ",
           substitute(lm.object), sep=""), cex=main.cex), ...
         ) {
  formula.lm <- formula(lm.object)
  x.name <- as.character(formula.lm[[3]])
  y.name <- as.character(formula.lm[[2]])
  missing.xlim <- missing(xlim)       ## R needs this
  missing.ylim <- missing(ylim)       ## R needs this
  missing.newdata <- missing(newdata) ## R needs this
  ## if.R(s={
  ##   ## Save a copy of the data.frame in frame=0 to put it where
  ##   ## model.frame.lm needs to find it when the example data is
  ##   ## run through Splus CMD check.
  ##   my.data.name <- as.character(lm.object$call$data)
  ##   if (length(my.data.name)==0)
  ##     stop("Please provide an lm.object calculated with an explicit 'data=my.data.frame' argument.")
  ##   undo.it <- (!is.na(match(my.data.name, objects(0))))
  ##   if (undo.it) old.contents <- get(my.data.name, frame=0)
  ##   my.data <- try(get(my.data.name))
  ##   if (class(my.data)=="Error")
  ##     my.data <- try(get(my.data.name, frame=sys.parent()))
  ##   if (class(my.data)=="Error")
  ##     stop("Please send me an email with a reproducible situation that got you here. (rmh@temple.edu)")
  ##   assign(my.data.name, my.data, frame=0)
  ## },r={})
  default.newdata <- data.frame(seq(xlim[1], xlim[2], length=51))
  names(default.newdata) <- x.name
  if (missing.xlim) xlim <- xlim + diff(xlim)*c(-.02,.02) ## needed
  if (missing.newdata) {
    newdata <- default.newdata
    newdata.x <- numeric()
  }
  else {
    if (is.na(match(x.name, names(newdata))))
      stop(paste("'newdata' must be a data.frame containing a column named '",
                 x.name, "'", sep=""))
    if (missing.xlim)
      xlim=range(xlim, newdata[[x.name]])
    newdata.x <- as.data.frame(newdata)[,x.name]
    newdata <- rbind(as.data.frame(newdata)[,x.name, drop=FALSE],
                     default.newdata)
    newdata <- newdata[order(newdata[,x.name]), , drop=FALSE]
  }
  if (missing.xlim) xlim <- xlim + diff(xlim)*c(-.02,.02) ## repeat is needed
  if (missing(newfit)) newfit <-
    ## if.R(s={

    ##   prediction <-
    ##     predict(lm.object, newdata=newdata,
    ##             se.fit=TRUE, ci.fit=TRUE, pi.fi=TRUE,
    ##             level=conf.level)
    ##   {
    ##     ## restore frame=0
    ##     if (undo.it) assign(my.data.name, old.contents, frame=0)
    ##     else remove(my.data.name, frame=0)
    ##   }
    ##   prediction
    ## }
    ##      ,r=
         {
           new.p <-
             predict(lm.object, newdata=newdata,
                     se.fit=TRUE, level=conf.level,
                     interval = "prediction")
           new.c <-
             predict(lm.object, newdata=newdata,
                     se.fit=TRUE, level=conf.level,
                     interval = "confidence")
           tmp <- new.p
           tmp$ci.fit <- new.c$fit[,c("lwr","upr"), drop=FALSE]
           dimnames(tmp$ci.fit)[[2]] <- c("lower","upper")
           attr(tmp$ci.fit,"conf.level") <- conf.level
           tmp$pi.fit <- new.p$fit[,c("lwr","upr"), drop=FALSE]
           dimnames(tmp$pi.fit)[[2]] <- c("lower","upper")
           attr(tmp$pi.fit,"conf.level") <- conf.level
           tmp$fit <- tmp$fit[,"fit", drop=FALSE]
           tmp
         }
##         )
  tpgsl <- trellis.par.get("superpose.line")
  tpgsl <- Rows(tpgsl, 1:4)
  tpgsl$col[1] <- 0
  if (missing.ylim) {
    ylim <- range(newfit$pi.fit, data[,y.name])
    ylim <- ylim + diff(ylim)*c(-.02,.02) ## needed
  }
  xyplot(formula.lm, data=data, newdata=newdata, newfit=newfit,
         newdata.x=newdata.x,
         xlim=xlim, ylim=ylim, pch=pch,
         panel=function(..., newdata.x) {
           panel.ci.plot(...)
           if (length(newdata.x) > 0)
             panel.rug(x=newdata.x)
         },
         main=main,
         key=list(border=TRUE,
           space="right",
           text=list(c("observed","fit","conf int","pred int")),
           points=list(
             pch=c(pch,32,32,32),
             col=c(trellis.par.get("plot.symbol")$col, tpgsl$col[2:4])
             ),
           lines=tpgsl),
         ...)
}

## source("~/HH-R.package/HH/R/ci.plot.R")
