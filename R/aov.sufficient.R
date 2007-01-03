aov.sufficient <-
  function(formula, data = NULL, projections = FALSE, qr = TRUE, contrasts = NULL,
           weights=data$n, sd=data$s, ...) {
    tmp <- sys.call()
    tmp[[1]] <- as.name("aov")
    tmp$sd <- NULL
    tmp$x <- TRUE
    result <- eval(tmp, sys.parent())
    result$df.residual <- sum(weights-1)
    Sq.res <- (weights-1)*sd^2
    result$residuals[] <- sqrt(sum(Sq.res)/sum(weights))
    result
}


## summary.lm does not give standard errors and t-tests for results
## calculated by aov.sufficient.  We need vcov.sufficient in R.
## It gives the same answer as the generic vcov when the generic vcov works.
if.R(r=
vcov.sufficient <- function(object, ...) {
  x <- object["x"][[1]] ## prevents partial matching with "xlevels" in R.
                        ## S-Plus doesn't have an "xlevels" component.
  if (length(x)==0) {
    x <- try(update(object, x=TRUE)$x, silent=TRUE)
    if (class(x)=="Error" || class(x)=="try-error") ## S-Plus || R
      stop("Please recompute the 'lm' object with 'x=TRUE'.")
  }
  xwx <-
    if (is.null(object$weights))
      crossprod(x)
    else
      crossprod(x, object$weights * x)
  solve(xwx)*anova(object)$`Mean Sq`[2]
}
,s={})
