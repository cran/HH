rowPcts <- function (x, ...)
{
    rSx <- rowSums(x, ...)
    result <- x/(rSx/100)
    Zero <- rSx == 0
    result[Zero,] <- 0
    structure(result, Sums = rSx)
}

colPcts <- function(x, ...)
  t(rowPcts(t(x, ...)))
