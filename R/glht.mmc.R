if.R(s={},
     r={
as.multicomp <- function (x, ...) 
  UseMethod("as.multicomp")

as.glht <- function (x, ...) 
  UseMethod("as.glht")

as.multicomp.glht <-
  function(x,       ## glht object
           focus,   ## currently required
           ylabel=deparse(terms(x$model)[[2]]),
           means=model.tables(x$model, type="means", cterm=focus)$tables[[focus]],
           height,
           lmat=t(x$linfct),
           lmat.rows=-1,
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           contrasts.none=FALSE,
           level=0.95,
           calpha=NULL,
           method=x$type,
           df,
           vcov.,
           ...
           ) {
    dimnames(x$linfct)[[1]] <- gsub(" ", "", dimnames(x$linfct)[[1]]) ## remove blanks
    if (dimnames(x$linfct)[[2]][1] == "") dimnames(x$linfct)[[2]][1] <- "(Intercept)"
    if (!missing(vcov.)) x$vcov <- vcov.(x$model)
    
    confint.x <-
      if (is.null(calpha))
        confint(x, level=level)  ##, ...)
      else
        confint(x, level=level, calpha=calpha)  ##, ...)
    
    result <- list(table=cbind(
                     estimate=confint.x$confint[,"Estimate"], #
                     stderr=0, ## placeholder
                     lower=confint.x$confint[,"lwr"], #
                     upper=confint.x$confint[,"upr"]  #
                     ),
                   alpha=1-attr(confint.x$confint,"conf.level"), #
                   error.type=NA,
                   method=method,                       #
                   crit.point=attr(confint.x$confint,"calpha"), #
                   Srank=NULL,
                   simsize=NULL,
                   ylabel=ylabel,
                   call=sys.call(),         #
                   lmcall=x$model,          #
                   focus=focus,             #
                   ## lmat=lmat,         #
                   glht=x
                   )
    result$table[,"stderr"] <-  ## correct stderr for the contrast
      (result$table[,"upper"] - result$table[,"lower"]) / (2*result$crit.point)
    if (is.null(dimnames(result$table)[[1]]))
      dimnames(result$table)[[1]] <- dimnames(confint.x$confint)[[1]]
    tmp <- lmat[lmat.rows, , drop=FALSE]
    if (contrasts.none) {
      first.row <- tmp[1,,drop=FALSE]
      first.row[] <- 0
      first.row[1,1] <- 1
    }
    else
      first.row <- -apply(tmp, 2, sum)
    lmat.subscript <- rbind(first=first.row, tmp)
    lmat.factor <- lmat.subscript

    lmat.factor <- sweep(lmat.factor, 2, apply(abs(lmat.factor), 2, sum)/2, "/")
    if (length(means) != nrow(lmat.factor))
      stop("Please specify lmat.rows with glht.mmc on a design with more than one factor.")
##    result$height <- (means %*% abs(lmat.factor))[1,]
    result$height <- height
    
    result$lmat <- 
      if (lmat.scale.abs2 && !contrasts.none)
        sweep(lmat, 2, apply(abs(lmat.subscript), 2, sum)/2, "/")
      else
        lmat
    if (order.contrasts)
      result <- multicomp.order(result)
    
    result$bounds <- switch(x$alternative,
                            "two.sided"="both",
                            "greater"="lower",
                            "less"="upper")
    class(result) <- c("multicomp.hh", "multicomp")

    result <- multicomp.reverse(result, estimate.sign)
    result$glht$linfct <- t(result$lmat)
    result
  }


as.glht.multicomp <- function(x, ...) x$glht

glht.mmc <- function (model, ...) 
  UseMethod("glht.mmc")

glht.mmc.lm <-
  function(model,       ## lm object
           linfct=NULL,
           focus=
           if (is.null(linfct))
           {
             if (length(model$contrasts)==1) names(model$contrasts)
             else stop("focus or linfct must be specified.")
           }
           else
           {
             if (is.null(names(linfct)))
               stop("focus must be specified.")
             else names(linfct)
           },
           focus.lmat,
           ylabel=deparse(terms(model)[[2]]),
           lmat=t(linfct),
           lmat.rows=-1,
           lmat.scale.abs2=TRUE,
           estimate.sign=1,
           order.contrasts=TRUE,
           level=.95,
           calpha=NULL,
           alternative = c("two.sided", "less", "greater"),
           ...
           ) {

##    if (model$contrasts[[focus]] != "contr.treatment")
    factors <- sapply(model$model, inherits, "factor")
    contrasts.type <- sapply(model$model[factors], attr, "contrasts")
    contrasts.type <- contrasts.type[!sapply(contrasts.type, is.null)]
    if (any(contrasts.type != "contr.treatment"))
      stop("glht.mmc requires an aov in which ALL factors use treatment contrasts.")
    
    result <- list(mca=NULL)

##  none.glht <- glht(model, linfct=mcp(focus.value="Means"))

    if (TRUE)
      {
        if (length(focus) > 1) stop("glht.mmc requires no more than one focus factor.")
        focus.linfct <- ## multcomp:::meanslinfct(
          meanslinfct.hh(  ## temporary until meanslinfct is changed
                         model, focus, formula=terms(model),
                         contrasts.arg=model$contrasts)
        none.glht <- glht(model, linfct=focus.linfct,
                          alternative=alternative, ...)
      }
    else
      {
        mcp.args <- list("Means")
        names(mcp.args) <- focus
        none.glht <- glht(model, linfct=do.call("mcp", mcp.args),
                          alternative=alternative)
      }
    
    means <-
      if (is.null(calpha)) {
        confint(none.glht, calpha=1.96)$confint[,"Estimate"] ## fake 1.96 ## , ...
      }
      else
        confint(none.glht, calpha=calpha)$confint[,"Estimate"] ## , ...
    
    
    if (is.null(linfct)) {
      linfct.focus <- mcalinfct(model, focus, linfct.Means=none.glht$linfct)
      method="Tukey"
    }
    else {
      if (match(focus, names(linfct), 0) != 0) {
        method <- attr(linfct[[focus]], "type")
        linfct.focus <-
##           if (is.matrix(linfct[[focus]]) &&
##               ncol(linfct[[focus]]) == length(coef(model)))
##             linfct[[focus]]
##           else
            linfct
      }
      else {
        linfct.focus <- linfct
        method <- NULL
      }
    }
## recover()
    mca.glht <- glht(model, linfct=linfct.focus,
                     alternative=alternative, ...)
    if (!is.null(method)) mca.glht$type <- method

    height.mca <-
      if (is.null(method) || method=="Tukey")
        means %*% abs(t(contrMat(table(model$model[[focus]]), "Tukey")))
      else
        means %*% abs(t(linfct.focus[[focus]])) ## fixme, this works for Dunnett
    
    result$mca <- as.multicomp(mca.glht, focus=focus, ylabel=ylabel,
                               means=means,
                               height=height.mca,
                               lmat.rows=lmat.rows,
                               lmat.scale.abs2=lmat.scale.abs2,
                               estimate.sign=estimate.sign,
                               order.contrasts=order.contrasts,
                               calpha=calpha,
                               level=level, ...)
    
    result$none <- as.multicomp(none.glht, focus=focus, ylabel=ylabel,
                                means=means,
                                height=means*2,
                                lmat=t(none.glht$linfct), lmat.rows=lmat.rows,
                                contrasts.none=TRUE, estimate.sign=0,
                                level=1-result$mca$alpha,
                                calpha=result$mca$crit.point,
                                method=result$mca$method, ...)
   if (!missing(lmat)) {
      if (lmat.scale.abs2) {
        tmp <- lmat[lmat.rows, , drop=FALSE]
        first.row <- -apply(tmp, 2, sum)
        lmat.subscript <- rbind(first=first.row, tmp)
        lmat <- sweep(lmat, 2, apply(abs(lmat.subscript), 2, sum)/2, "/")
      }
      lmat.glht <- glht(model, linfct=t(lmat),
                        alternative=alternative, ...)

      result$lmat <- as.multicomp(lmat.glht, focus=focus, ylabel=ylabel,
                                  means=means,
                                  height=means %*% abs(sweep(focus.lmat, 2, apply(abs(focus.lmat), 2, sum)/2, "/")),
                                  lmat=lmat, lmat.rows=lmat.rows,
                                  level=1-result$mca$alpha,
                                  calpha=result$mca$crit.point,
                                  method=result$mca$method, ...)
    }

    class(result) <- "mmc.multicomp"
    result
  }

glht.mmc.glht <- function(model, ...) {
##  do.call("glht.mmc.lm", c(list(model=model$model), list(...)))
NextMethod("glht.mmc", model$model)
}



## prints glht components of mmc.multicomp object
print.glht.mmc.multicomp <- function (x, ...) {
  cat(paste("Fit:", deparse(x$mca$glht$model$call, width.cutoff=500), "\n"))
  cat("Focus =", x$mca$focus, "\n")
  cat("Estimated Quantile =", x$mca$crit.point, "\n")
  cat(round((1-x$mca$alpha)*100), "% family-wise confidence level\n", sep="")
  tmp <- list(mca = x$mca$glht, none = x$none$glht)
  if (is.null(tmp$none)) tmp$none <- x$none$table
  if (!is.null(x$lmat)) 
    tmp$lmat <- x$lmat$glht
  print(tmp)
  invisible(x)
}


## prints table and height components of multicomp object
print.multicomp <- function (x, ...) {
  print(cbind(x$table, height=x$height/2))
  invisible(x)
}

## print.multicomp.hh is in print.multicomp.hh.s


## prints table and height components of each multicomp object in a mmc object
print.mmc.multicomp <- function (x, ...) {
  cat(paste(x$mca$method, "contrasts\n"))
  cat(paste("Fit:", deparse(x$mca$glht$model$call, width.cutoff=500), "\n"))
  cat("Estimated Quantile =", x$mca$crit.point, "\n")
  cat(round((1-x$mca$alpha)*100), "% family-wise confidence level\n", sep="")
  cat("$mca\n")
  print(x$mca)
  cat("$none\n")
  print(x$none)
  if (!is.null(x$lmat)) {
    cat("$lmat\n")
   print(x$lmat)
  }
  invisible(x)
}

plot.multicomp <- function (x, ...) {
  plot(confint(as.glht(x)), ...)
}

## plot.multicomp.hh is in file plot.multicomp.R
})


## source("~/HH-R.package/HH/R/glht.mmc.R")

## c.mmc <- glht.mmc(catalystm1.aov, linfct = mcp(catalyst = "Tukey"))




##            focus.columns=
##            match(focus, attr(terms(model), "term.labels")) ==
##            attr(model$qr$qr, "assign"), 
