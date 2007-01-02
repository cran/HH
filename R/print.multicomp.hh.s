## HH extension to S-Plus print.multicomp to include the height column
## HH version uses digits=4 as the default.

print.multicomp.hh <-
  if.R(s=
  function(x, digits=4, ..., height=T)
{
  labels <- dimnames(x$table)[[1]]
  estimate <- signif(x$table[, "estimate"], digits)
  stderr <- signif(x$table[, "stderr"], digits)
  flaggit <- ifelse((x$table[, "lower"] > 0 | x$table[, "upper"] < 0),
                    "****", "    ")
  lower <- signif(x$table[, "lower"], digits)
  upper <- signif(x$table[, "upper"], digits)
  if(x$error.type == "fwe")
    error.type <- "simultaneous"
  else error.type <- "non-simultaneous"
  if(all(is.finite(lower)) & all(is.finite(upper)))
    bound.type <- "intervals"
  else bound.type <- "bounds"
  method <- switch(x$method,
                   lsd = "Fisher LSD",
                   tukey = "Tukey",
                   dunnett = "Dunnett",
                   sidak = "Sidak",
                   bon = "Bonferroni",
                   scheffe = "Scheffe",
                   sim = "simulation-based",
                   tpmc = "Cheung and Chan")
  if (is.null(method) && is.null(x$method))
    method <- "user-specified"
  else
    method <- x$method
  cat("\n")
  cat(as.character(100 * (1 - x$alpha)), "%", error.type, "confidence",
      bound.type, "for specified", "\n")
  cat("linear combinations, by the", method, "method", "\n")
  cat("\n")
  cat("critical point:", round(x$crit.point, 4), "\n")
  cat("response variable:", x$ylabel, "\n")
  if(x$method == "scheffe")
    cat("rank used for Scheffe method:", as.character(x$Srank), "\n")
  if(x$method == "sim")
    cat("simulation size=", as.character(x$simsize), "\n")
  cat("\n")
  cat(bound.type, "excluding 0 are flagged by '****'", "\n")
  cat("\n")
  if(!all(is.finite(upper))) {
    table <- data.frame(estimate, stderr, lower, flaggit)
    names(table) <- c("Estimate", "Std.Error", "Lower Bound", "")
  }
  else if(!all(is.finite(lower))) {
    table <- data.frame(estimate, stderr, upper, flaggit)
    names(table) <- c("Estimate", "Std.Error", "Upper Bound", "")
  }
  else {
    table <- data.frame(estimate, stderr, lower, upper, flaggit)
    names(table) <- c("Estimate", "Std.Error", "Lower Bound", "Upper Bound", "")
  }
  row.names(table) <- labels
  if (height && !is.null(x$height))
    table$"Mean Height" <- signif(x$height/2, digits)
  print(table, ...)
  cat("\n")
}
,r=
  function (x, ...) {
    NextMethod("print")
  }
)

## source("~/HH-R.package/HH/R/print.multicomp.hh.s")
## trace(print.multicomp, exit=browser)
