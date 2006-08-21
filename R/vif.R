"vif" <-
function(x, ...)
  UseMethod("vif")

"vif.default" <-
function(x, y.name, na.action=na.exclude, ...) {
  nnames <- names(x)
  nn.x <- seq(along=nnames)
  if (missing(y.name))
    y.number <- 0
  else {
    y.number <- match(y.name, nnames, 0)
    nn.x <-  nn.x[-y.number]
  }
  r2 <- nn.x
  names(r2) <- nnames[nn.x]
  if (length(r2) < 2) stop("vif requires two or more X-variables.")
  for (i in nn.x) {
    tmp.lm <- lm(x[,i] ~
                 data.matrix(x[,-c(y.number, i)]),
                 na.action=na.action)
  r2[nnames[i]] <- summary(tmp.lm)$r.squared
  }
  1/(1-r2)
}

"vif.formula" <-
function(x, data, na.action=na.exclude, ...) {
  vif(lm(x, data, na.action=na.action, x=TRUE))
}

"vif.lm" <-
function(x, na.action=na.exclude, ...) {
  if(is.null(x$x) || length(x$x)==0)
    x <- update(x, x = TRUE)
  x <- as.data.frame(unclass(x$x))[-1]
  vif(x, na.action=na.action)
}
