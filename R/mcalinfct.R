## MCA multiple comparisons analysis (pairwise)
if.R(r=
mcalinfct <-
  function(model, focus,
           mmm.data=model$model,
           formula.in=terms(model),
           linfct.Means=
           ## multcomp:::meanslinfct(
           meanslinfct.hh(  ## temporary until meanslinfct is changed
                          model, focus, mmm.data, formula.in,
                          contrasts.arg=model$contrasts),
           type="Tukey"
           ) {
    lev <- levels(mmm.data[[focus]])
    names(lev) <- lev
    mca <- contrMat(lev, type)
##     result <-
    mca[, lev] %*% linfct.Means[lev,]
##     attr(result, "type") <- "Tukey"
##     attr(result, "class") <- c("contrMat", "matrix")
##     mca <- list(result)
##     names(mca) <- focus
##     attr(mca, "class") <- "mcp"
##     mca
  }
,s={})

## source("~/HH-R.package/HH/R/mcalinfct.R")
