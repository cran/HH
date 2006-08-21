## plot x and y,
## with straight line fit and display of squared residuals
regr1.plot <- function(x, y, model=lm(y~x), coef.model=coef(model),
                       main="put a useful title here",
                       xlab=deparse(substitute(x)),
                       ylab=deparse(substitute(y)),
                       jitter.x=FALSE,
                       resid.plot=FALSE,
                       points.yhat=TRUE,
                       ..., length.x.set=51,
                       err=-1) {
  old.err <- par(err=err)
  if (jitter.x) x <- jitter(x)
  plot(x, y, xlab=xlab, ylab=ylab, main=main, ...)
  x.set <- seq(par()$usr[1], par()$usr[2], length=length.x.set)
  newdata <- list(x=x.set)
  names(newdata) <- names(coef(model))[2]
  y.hat.set <- predict(model, newdata)
  lines(x.set, y.hat.set)

  if (missing(model)) {
    if (is.list(coef.model)) coef.model <- coef.model[["coef"]]
    y.hat <- coef.model[1] + coef.model[2] * x
  }
  else
    y.hat <- predict(model)
  
  if (points.yhat) points(y=y.hat, x=x, pch=16)
  if (resid.plot != FALSE)
    resid.squares(x, y, y.hat, resid.plot)
  par(old.err)
  invisible(NULL)
}
