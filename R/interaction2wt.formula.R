"interaction2wt.formula" <-
function(x, data=sys.parent(), responselab,
			    ...) {
  if.R(
       r={
         dft <- do.formula.trellis.xysplom(x, data=data)
         y.in <- dft$y[[1]]
         x.in <- dft$x
         responselab <- names(dft$y)
       },
       s={
         dft <- do.formula.trellis(x)
         y.in <- eval(dft$expr[[1]], local=data)
         x.in <- data[,dft$xlab]
         responselab <- dft$ylab
       })
  if (is.null(x.in) || is.null(y.in))
    stop("both x and y are needed in formula")
  interaction2wt.default(x=x.in, response.var=y.in,
                              responselab=responselab,
                              ...)
}
