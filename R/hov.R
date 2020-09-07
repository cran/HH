"hov" <-
function(x, data=NULL, method="bf") {
  if (method != "bf")
    stop("Only 'bf', Brown-Forsyth method is currently available.")
    do.formula.trellis <- NA ## make R-2.6.0dev happy
    lPF <- latticeParseFormula(x, data=data)
    y <- lPF$left
    group <- lPF$right
    y.name <- lPF$left.name
    group.name <- lPF$right.name
  hov.bf(y, group, y.name, group.name)
}

"hov.bf" <-
function(x, group,
                   y.name=deparse(substitute(x)),
                   group.name=deparse(substitute(group))) {
  med <- tapply(x, group, median)
  z <- abs(x - med[group])
  hov.test <- summary(aov(z ~ group))
  hov.test <- hov.test[[1]]
  result <- list(statistic=hov.test[1,4],
                 parameters=hov.test[,"Df"],
                 p.value=hov.test[1,5],
                 alternative="variances are not identical",
                 method="hov: Brown-Forsyth",
                 data.name=y.name)
  names(result$statistic) <- "F"
  names(result$parameters) <- dimnames(hov.test)[[1]]
  names(result$parameters)[1] <- group.name
  names(result$parameters) <- paste("df", sep=":", names(result$parameters))
  class(result) <- "htest"
  result
}
