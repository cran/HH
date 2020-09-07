orthog.complete <- function(x, normalize=TRUE, abs2.rows=1:nrow(x),
                            Int=TRUE, drop.Int=Int) {
  if (length(dimnames(x)[[2]])==0) dimnames(x)[[2]] <- paste("Col",1:ncol(x), sep=".")
  if (Int) x <- cbind("(I)"=1, x)
  qr.x <- qr(x)
  Q <- qr.Q(qr.x, complete=TRUE)
  dimnames(Q) <- list(paste("Row",1:nrow(Q), sep="."),
                      paste("Col",1:ncol(Q), sep="."))
  if (length(dimnames(x)[[1]])) dimnames(Q)[[1]] <- dimnames(x)[[1]]
  mincolx <- min(ncol(x), ncol(Q))
  if (length(dimnames(x)[[2]])) dimnames(Q)[[2]][1:mincolx] <- dimnames(x)[[2]][1:mincolx]
  if (drop.Int) Q <- Q[,-1, drop=FALSE]
  if (normalize=="abs2")
    sweep(Q, 2, apply(abs(Q[abs2.rows,,drop=FALSE]), 2, sum)/2, "/")
  else  Q
}


orthog.construct <- function(y, x, x.rows, normalize=FALSE) {
  y.x.lm <- lm(y ~ x[x.rows,, drop=FALSE] - 1, singular.ok=TRUE, qr=TRUE)
  assign <- y.x.lm$qr$pivot[seq(y.x.lm$qr$rank)]
  beta.tmp <- coef(y.x.lm)
  beta <-
    if.R(s={
      array(beta.tmp, dim(beta.tmp), dimnames(beta.tmp)) ## force S-Plus "coef" class to "matrix"
    },r={
      beta.tmp[assign,, drop=FALSE] ## subscript out any NA values
    })
  result <- x[,assign, drop=FALSE] %*% beta
  if (normalize=="abs2")
    sweep(result, 2, apply(abs(result[x.rows,,drop=FALSE]), 2, sum)/2, "/")
  else result
}

## trace(orthog.complete, exit=browser)
## trace(orthog.construct, exit=browser)
