"ci.plot.lm" <-
function(lm.object,
         xlim=range(data[,as.character(formula.lm[[3]])]),
         newdata=data.frame(seq(xlim[1], xlim[2], length=51)),
         conf.level=.95,
         data=model.frame(lm.object),
         newfit,
         ylim=range(newfit$pi.fit),
         pch=16,
         main.cex=1,
         main=list(paste(100*conf.level,
           "% confidence and prediction intervals for ",
           substitute(lm.object), sep=""), cex=main.cex), ...
         ) {
  formula.lm <- formula(lm.object)
  if (missing(xlim)) xlim <- xlim + diff(xlim)*c(-.02,.02) ## needed
  if (missing(newdata)) names(newdata) <- as.character(formula.lm[[3]])
  if (!missing(newdata) && missing(xlim)) xlim=range(x, newdata[[1]])
  if (missing(xlim)) xlim <- xlim + diff(xlim)*c(-.02,.02) ## repeat is needed
  if (missing(newfit)) newfit <-
    if.R(s=
         predict(lm.object, newdata=newdata,
                 se.fit=TRUE, ci.fit=TRUE, pi.fi=TRUE,
                 conf.level=conf.level)
         ,r={
           new.p <-
             predict(lm.object, newdata=newdata,
                     se.fit=TRUE, conf.level=conf.level,
                     interval = "prediction")
           new.c <-
             predict(lm.object, newdata=newdata,
                     se.fit=TRUE, conf.level=conf.level,
                     interval = "confidence")
           tmp <- new.p
           tmp$ci.fit <- new.c$fit[,c("lwr","upr")]
           dimnames(tmp$ci.fit)[[2]] <- c("lower","upper")
           attr(tmp$ci.fit,"conf.level") <- conf.level
           tmp$pi.fit <- new.p$fit[,c("lwr","upr")]
           dimnames(tmp$pi.fit)[[2]] <- c("lower","upper")
           attr(tmp$pi.fit,"conf.level") <- conf.level
           tmp$fit <- tmp$fit[,"fit"]
           tmp
         })
  tpgsl <- trellis.par.get("superpose.line")
  tpgsl <- lapply(tpgsl, function(x) x[1:4])
  tpgsl$col[1] <- 0
  xyplot(formula.lm, data=data, newdata=newdata, newfit=newfit,
         xlim=xlim, ylim=ylim, pch=pch,
         panel="panel.ci.plot",
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

