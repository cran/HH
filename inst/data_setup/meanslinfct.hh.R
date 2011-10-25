## based on  multcomp:::meanslinfct
## (which I also wrote)

meanslinfct.hh <-
function (model, focus, mmm.data = model$model, formula.in = terms(model),
          contrasts.arg = NULL) 
{
    mmm.factor <- sapply(mmm.data, inherits, "factor")
    mmm.levels <- lapply(mmm.data[mmm.factor], levels)
    mmm.rows <- sapply(mmm.levels, length)
    n.mmm.rows <- prod(mmm.rows)
    mmm.new <- mmm.data[1:n.mmm.rows, ]
    mmm.factor.names <- names(mmm.data)[mmm.factor]
    mmm.rows.forward <- cumprod(mmm.rows)
    mmm.rows.forward.prev <- c(1, mmm.rows.forward)
    names(mmm.rows.forward.prev) <- c(names(mmm.rows.forward), 
        "all")
    for (i in mmm.factor.names) mmm.new[[i]] <- gl(mmm.rows[i], 
        mmm.rows.forward.prev[i], n.mmm.rows, labels = mmm.levels[[i]])
    mmm.numeric.names <- names(mmm.data)[!mmm.factor]
    for (i in mmm.numeric.names) mmm.new[[i]][] <- mean(mmm.data[[i]])
    none.data <- model.matrix(formula.in, data = mmm.new,
                              contrasts.arg=contrasts.arg)
    none.linfct <- aggregate(none.data, by = mmm.new[focus], 
        FUN = mean)[, -1]
    rownames(none.linfct) <- levels(mmm.new[[focus]])
    data.matrix(none.linfct)
}

## source("~/HH-R.package/HH/R/meanslinfct.hh.R")
