## MCA multiple comparisons analysis (pairwise)
if.R(r=
mcalinfct <-
  function(model, focus,
           mmm.data=model$model,
           formula.in=terms(model),
           linfct.Means=
           multcomp:::meanslinfct(model, focus, mmm.data, formula.in),
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
