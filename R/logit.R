### logit and antilogit

logit <- function(p) log(p/(1-p))

##antilogit <- function(x) exp(x)/(1+exp(x))  #boundary problem when x==Inf

antilogit <- function(x) {
  tmp <- (x != Inf)
  result <- x
  result[tmp] <- exp(x[tmp])/(1+exp(x[tmp]))
  result[!tmp] <- 1
  result
}
