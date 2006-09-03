## Take the result of a multiple comparisons analysis from simint
## and force all comparisons to have the same sign.
## If estimate.sign==1, reverse the negatives.
## If estimate.sign==-1, reverse the positives.
## Both the names of the comparisons and the numerical values are reversed.
## If estimate.sign==0, return the argument.

## I have used the estimate.sign=0 argument only once ever.
## I did so in ~/504.s06/0316/filter.s and that is because there
## were three factors and one of them had an implicit ordering.


simint.reverse <- function(y, estimate.sign=1, ...) {
  hmtest <- y
  if (estimate.sign == 0) return(hmtest)   ## no change, return argument
  if (estimate.sign > 0)
    tml <- (hmtest$estimate < 0) ## locate negatives to be changed
  else
    tml <- (hmtest$estimate > 0) ## locate positives to be changed

  hmtest$cmatrix[tml,]            <- -hmtest$cmatrix[tml,,drop=FALSE]
  hmtest$estimate[tml,]           <- -hmtest$estimate[tml,,drop=FALSE]
  hmtest$statistics[tml,]         <- -hmtest$statistics[tml,,drop=FALSE]
  hmtest.conf.int.tml..upper      <-  hmtest$conf.int[tml,"upper",drop=FALSE] 
  hmtest$conf.int[tml,"upper"]    <- -hmtest$conf.int[tml,"lower",drop=FALSE] 
  hmtest$conf.int[tml,"lower"]    <- -hmtest.conf.int.tml..upper
  
  contrast.names <- dimnames(hmtest$estimate)[[1]][tml]
  minus.position <- regexpr("-",contrast.names)
  if (any(minus.position==-1))
    warning("At least one reversed contrast name did not have a '-' sign.  We appended a '-' sign.")
  last.position  <- nchar(contrast.names)
  contrast.names.rev <-
    paste(substring(contrast.names, minus.position+1, last.position),
          substring(contrast.names, 1, minus.position-1),
          sep="-")
  
  dimnames(hmtest$cmatrix)[[1]][tml]     <- contrast.names.rev
  dimnames(hmtest$estimate)[[1]][tml]    <- contrast.names.rev
  names(hmtest$sd)[tml]                  <- contrast.names.rev
  dimnames(hmtest$statistics)[[1]][tml]  <- contrast.names.rev
  dimnames(hmtest$p.value.raw)[[1]][tml] <- contrast.names.rev
  names(hmtest$p.value.adj)[tml]         <- contrast.names.rev
  dimnames(hmtest$conf.int)[[1]][tml]    <- contrast.names.rev

  hmtest
}
