"as.multicomp" <-
function(x, ...)
  UseMethod("as.multicomp")

"as.multicomp.hmtest" <-
function(x,
         group="group",
         focus=x$FNAMES$mainF,
         ylabel=x$FNAMES$response,
         lmcall=x$call,
         means,
         lmat=t(x$cmatrix),
         lmat.rows=seq(along=dimnames(lmat)[[1]])[-1],
         aov.mca,
         ...) {
    if (dimnames(lmat)[[1]][1] == "") dimnames(lmat)[[1]][1] <- "(Intercept)"
    result <- list(table=cbind(
                     estimate=x$estimate[,1],
                     stderr=x$sd,
                     x$conf.int
                     ),
                   alpha=1-attr(x$conf.int,"conf.level"),
                   error.type=NA,
                   method=x$ctype,
                   crit.point=x$calpha,
                   Srank=NULL,
                   simsize=NULL,
                   ylabel=ylabel,
                   call=sys.call(),
                   lmcall=lmcall,
                   focus=focus,
                   lmat=lmat
                   )
    result$bounds <- switch(x$alternative,
                                 "two.sided"="both",
                                 "greater"="lower",
                                 "less"="upper")
    result$height=(means %*% abs(lmat[lmat.rows,]))[1,]
    dimnames(result$table)[[2]][3:4] <- c("lower","upper")
    class(result) <- "multicomp"
    multicomp.order(result)
  }

## "as.mmc.multicomp" <-
## function(x, ...)
##   UseMethod("as.mmc.multicomp")

"print.mmc.multicomp" <-
function(x, ...) {
  tmp <- list(mca=x$hmtest, none=x$none.hmtest)
  if (!is.null(x$lmat.hmtest)) tmp$lmat <- x$lmat.hmtest
  print(tmp)
  invisible(x)
}

plot.multicomp <- function(x, ...)
  plot(as.hmtest.multicomp(x), ...)

"as.hmtest" <-
function(x, ...)
  UseMethod("as.hmtest")

"as.hmtest.multicomp" <-
  function(x, eps=.001, ...) {
    result <- list(estimate=as.matrix(x$table[,"estimate"]),
                   sd=x$table[,"stderr"],
                   conf.int=x$table[,3:4],
                   ctype=x$method,
                   calpha=x$crit.point,
                   call=x$lmcall,
                   cmatrix=t(x$lmat),
                   FNAMES=list(mainF=x$focus.name, response=x$ylabel),
                   height=x$height,
                   eps=eps,
                   asympt=FALSE
                   )
    attr(result$conf.int,"conf.level") <- 1-x$alpha
    result$alternative <- switch(x$bounds,
                                 "both"="two.sided",
                                 "lower"="greater",
                                 "upper"="less")
    class(result) <- "hmtest"
    result
  }
