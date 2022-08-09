InsertVerticalPanels <- function(x, expansion, newRowheights=5, newValue=NA) {
  if (To3 <- (length(dim(x)) == 2)) {
    x <- array(x, dim=c(1, dim(x)), dimnames=c(list("ONE"), dimnames(c)))
  }
  ce <- cumsum(expansion)
  if (expansion[1]==0) ce[1] <- 1
  result <- x[, ce, , drop=FALSE]
  newRows <- which(expansion == 0)
  result[, newRows, ] <- 0
  for (i in seq_len(length(newRows)))
    dimnames(result)[[2]][newRows[i]] <- paste0(rep(" ", i), collapse="")
  rowheights <- rowSums(result[1, , ])
  result[, newRows, ] <- newValue
  rowheights[newRows] <- newRowheights
  if (To3) {
    result2 <- result
    dim(result2) <- dim(result)[2:3]
    dimnames(result2) <- dimnames(result)[2:3]
    result <- result2
  }
  attr(result, "rowheights") <- rowheights
  attr(result, "newRows") <- newRows
  result
}
