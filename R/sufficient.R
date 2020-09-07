sufficient <- function(x,
                       yname=dimnames(x)[[2]][[1]],
                       factor.names.keep=dimnames(x)[[2]][-c(1,2)]) {

  tmp <-
  tapply(x[,yname], x[,factor.names.keep],
         function(x) c(mean=mean(x, na.rm=TRUE),
                       sd=sd(x, na.rm=TRUE),
                       nobs=length(x)))
  tmp <- lapply(tmp, function(x) if (is.null(x)) c(NA, NA, 0) else x)
  tmp <- do.call("rbind", tmp)
  dimnames(tmp)[[2]][1] <- yname

  for (i in factor.names.keep)
    if (is.numeric(x[[i]]))
      x[[i]] <- ordered(x[[i]], sort(unique(x[[i]])))
  factors <- expand.grid(lapply(x[,factor.names.keep],
                                function(x) levels(as.factor(x))))
  for (i in factor.names.keep)
    attributes(factors[[i]]) <- attributes(x[[i]])

  cbind(factors, tmp)
}

## trace(sufficient, exit=browser)
