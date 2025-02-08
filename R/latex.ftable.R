## > stats:::write.ftable
## function (x, file = "", quote = TRUE, append = FALSE, digits = getOption("digits"),
##     ...)
## {
##     r <- format.ftable(x, quote = quote, digits = digits, ...)
##     cat(t(r), file = file, append = append, sep = c(rep(" ",
##         ncol(r) - 1), "\n"))
##     invisible(x)
## }
## <bytecode: 0x7fa386e911f8>
## <environment: namespace:stats>

## latexSetOptions()


latex.ftable <- function(object, ...,
                         style=c("ftableX", "array", "ftable"),
                         quote=FALSE, digits=getOption("digits")) {
  style <- match.arg(style)

  result <- unclass(object)

  rv <- attr(object, "row.vars")
  n.row.vars <- length(rv)
  dimrv <-  sapply(rv, length)
  rg <-rep(dimrv[n.row.vars], prod(dimrv[-n.row.vars]))

  if (!is.null(list(...)$rgroup)) row.names(object) <- 1:nrow(object)


  cv <- attr(object, "col.vars")
  n.col.vars <- length(cv)
  dimcv <-  sapply(cv, length)
  cg <-rep(dimcv[n.col.vars], prod(dimcv[-n.col.vars]))



  dimobj <- dim(object)

  ## recover()

  switch(style,
         ftableX={
           r <- format(object, ...,
                       quote = quote, digits = digits)
           latex(r,
                 n.cgroup=c(n.row.vars+1, cg),
                 n.rgroup=c(n.col.vars+1, rg),
                 ...)
         },
         array={
           latex(object)
         },
         ftable={
           r <- format(object, ...,
                       quote = quote, digits = digits)
           latex(r)
         }
         )
}




if (FALSE) {

     ft31 <- ftable(Titanic, row.vars = 3:1, col.vars = 4)
     write.ftable(ft31, quote = FALSE)
     write.ftable(ft31, quote = FALSE, method="row.compact")
     write.ftable(ft31, quote = FALSE, method="col.compact")
     write.ftable(ft31, quote = FALSE, method="compact")
latex(ft31, style="ftable")
latex(ft31, style="ftableX")
latex(ft31, style="ftableX", rgroup=c("","A","B","C","D"))
latex(ft31, style="array")
latex(ft31)

       ft13 <- ftable(Titanic, row.vars = 1, col.vars = 4:2)
     write.ftable(ft13, quote = FALSE)
     write.ftable(ft13, quote = FALSE, method="row.compact")
     write.ftable(ft13, quote = FALSE, method="col.compact")
     write.ftable(ft13, quote = FALSE, method="compact")
latex(ft13, style="ftableX", cgroup=c("","A","B","C","D"))
latex(ft13, style="ftableX")
latex(ft13, style="ftable")
latex(ft13, style="array")

}
