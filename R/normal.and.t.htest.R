NormalAndTplot.htest <- function(mean0, type="hypothesis", xlim=NULL, ...,
                               xbar, sd, df, n, alpha.left, alpha.right, distribution.name, sub ## these input arguments will be ignored
                               ) {
  ## this function takes "htest" objects from base::t.test and TeachingDemos::z.test
  t.htest <- mean0
  tstat <- t.htest$statistic
  distribution.name <- names(tstat)
  if (!distribution.name %in% c("t","z")) stop("t or z tests only", call.=FALSE)
  estimate <- t.htest$estimate
  xbar <- switch(length(estimate),
                 estimate,
                 estimate[1] - estimate[2])
  parameter <- t.htest$parameter
  df <- parameter["df"]
  if (is.null(df) || is.na(df)) df <- Inf
  n <- parameter["n"]
  if (is.null(n) || is.na(n)) n <- 1
  sd <- (xbar-t.htest$null.value) / tstat
  if (n > 1) sd <- sd*sqrt(n)
  switch(t.htest$alternative,
         two.sided={
           alpha.left <- (1-attr(t.htest$conf.int, "conf.level"))/2
           alpha.right <- alpha.left
           sided <- "both"
           if (is.null(xlim))
             xlim <- if (type=="hypothesis")
                       1.12*c(-1,1)*max(abs(c(xbar, diff(t.htest$conf.int))))
                     else
                       xbar + .8*c(-1,1)*diff(t.htest$conf.int)
         },
         less={
           if (type=="hypothesis") {
             alpha.left <- 1-attr(t.htest$conf.int, "conf.level")
             alpha.right <- 0
             sided <- "left"
             if (is.null(xlim))
               xlim <- c(min(xbar, t.htest$null.value - 3.5*sd),
                         max(t.htest$conf.int[2], t.htest$null.value + 3.5*sd, xbar))
           }
           else { ## type == "confidence"
             alpha.left <- 0
             alpha.right <- 1-attr(t.htest$conf.int, "conf.level")
             sided <- "right"
             if (is.null(xlim))
               xlim <- xbar + c(-1,1)*3.5*sd
           }
         },
         greater={
           if (type=="hypothesis") {
             alpha.left <- 0
             alpha.right <- 1-attr(t.htest$conf.int, "conf.level")
             sided <- "right"
             if (is.null(xlim))
               xlim <- c(min(t.htest$conf.int[1], t.htest$null.value - 3.5*sd, xbar),
                         max(t.htest$null.value + 3.5*sd, xbar))
           }
           else { ## type == "confidence"
             alpha.left <- 1-attr(t.htest$conf.int, "conf.level")
             alpha.right <- 0
             sided <- "left"
             if (is.null(xlim))
               xlim <- xbar + c(-1,1)*3.5*sd
           }
         }
         )

  sub <- t.htest$method

  NormalAndTplot(xbar=xbar, sd=sd, df=df, n=n, alpha.left=alpha.left, alpha.right=alpha.right,
               xlim=xlim, distribution.name=distribution.name, sub=sub, type=type, ...)
}

if (FALSE) {
  ## x1 <- rnorm(12)
  ## x2 <- rnorm(17)
  ## > dump("x1","")
  x1 <-
    c(1.54879522444146, 0.181710578336539, -1.11207118177499, -1.34347686059896,
      -0.24108507822502, -0.91047940515944, 1.45511349214956, -1.39692329196256,
      -1.47887647837318, -0.192103462564679, 0.685862277883836, -0.782858996378826
      )
  ## > dump("x2","")
  x2 <-
    c(-0.663607021665983, -0.417067146171893, -1.52688809034017,
      -0.971622426178958, 1.70694060811116, -0.0704891692673179, -2.4546096840876,
      0.394500217027823, -0.0641578142663866, 1.22556246421403, -0.202044044778545,
      -1.77204722685881, -0.441276374986494, 0.559378515165325, -0.651640190817702,
      -0.379993366989808, 0.392024083760207)

  ## t.12 <- t.test(x1, x2)
  ## t.12
  ## print.default(t.12)
  ##
  ## NormalAndTplot(t.12)
  ## print(NormalAndTplot(t.12), tables=TRUE)
  ##
  ## > print.default(t.12)
  ## $statistic
  ##          t
  ## 0.03777655

  ## $parameter
  ##       df
  ## 23.32924

  ## $p.value
  ## [1] 0.9701872

  ## $conf.int
  ## [1] -0.8099133  0.8400678
  ## attr(,"conf.level")
  ## [1] 0.95

  ## $estimate
  ##  mean of x  mean of y
  ## -0.2988661 -0.3139433

  ## $null.value
  ## difference in means
  ##                   0

  ## $alternative
  ## [1] "two.sided"

  ## $method
  ## [1] "Welch Two Sample t-test"

  ## $data.name
  ## [1] "x1 and x2"

  ## attr(,"class")
  ## [1] "htest"
  ## >

  t.1 <- t.test(x1)
  t.1
  t.1.nt <- NormalAndTplot(t.1)
  t.1.nt
  cat(attr(t.1.nt, "call"), "\n")
  attr(t.1.nt, "scales")
  attr(t.1.nt, "prob")
  print(t.1.nt, call=TRUE)

  ## two-sample two-sided
  t.122 <- t.test(x1, x2+2)
  t.122
  print.default(t.122)
  NormalAndTplot(t.122)
  print(NormalAndTplot(t.122), tables=TRUE, digits=8)
  NormalAndTplot(t.122, type="confidence")
  print(NormalAndTplot(t.122, type="confidence"), tables=TRUE, digits=8)

  ## one-sample two-sided
  t.1 <- t.test(x1)
  t.1
  print.default(t.1)
  NormalAndTplot(t.1)
  print(NormalAndTplot(t.1), tables=TRUE, digits=8)
  NormalAndTplot(t.1, type="confidence")
  print(NormalAndTplot(t.1, type="confidence"), tables=TRUE, digits=8)

  ## two-sample right-sided
  t.12r <- t.test(x1, x2+2, alternative="greater")
  t.12r
  print.default(t.12r)
  NormalAndTplot(t.12r)
  print(NormalAndTplot(t.12r), tables=TRUE, digits=8)
  NormalAndTplot(t.12r, type="confidence")
  print(NormalAndTplot(t.12r, type="confidence"), tables=TRUE, digits=8)

  t.12rm <- t.test(x1, x2-2, alternative="greater")
  t.12rm
  print.default(t.12rm)
  NormalAndTplot(t.12rm)
  print(NormalAndTplot(t.12rm), tables=TRUE, digits=8)
  NormalAndTplot(t.12rm, type="confidence")
  print(NormalAndTplot(t.12rm, type="confidence"), tables=TRUE, digits=8)


  ## one-sample right-sided
  t.1r <- t.test(x1, alternative="greater")
  t.1r
  print.default(t.1r)
  NormalAndTplot(t.1r)
  print(NormalAndTplot(t.1r), tables=TRUE, digits=8)
  NormalAndTplot(t.1r, type="confidence")
  print(NormalAndTplot(t.1r, type="confidence"), tables=TRUE, digits=8)

  t.1rb <- t.test(x1+1, alternative="greater")
  t.1rb
  print.default(t.1rb)
  NormalAndTplot(t.1rb)
  print(NormalAndTplot(t.1rb), tables=TRUE, digits=8)
  NormalAndTplot(t.1rb, type="confidence")
  print(NormalAndTplot(t.1rb, type="confidence"), tables=TRUE, digits=8)

  ## two-sample left-sided
  t.12l <- t.test(x1+1, x2, alternative="less")
  t.12l
  print.default(t.12l)
  NormalAndTplot(t.12l)
  print(NormalAndTplot(t.12l), tables=TRUE, digits=8)
  NormalAndTplot(t.12l, type="confidence")
  print(NormalAndTplot(t.12l, type="confidence"), tables=TRUE, digits=8)

  ## one-sample left-sided
  t.1l <- t.test(x1, alternative="less")
  t.1l
  print.default(t.1l)
  NormalAndTplot(t.1l)
  print(NormalAndTplot(t.1l), tables=TRUE, digits=8)
  NormalAndTplot(t.1l, type="confidence")
  print(NormalAndTplot(t.1l, type="confidence"), tables=TRUE, digits=8)


  library(TeachingDemos)
  zb <- z.test(x1+1, stdev=2)
  zb
  print(NormalAndTplot(zb), tables=TRUE)
  print(NormalAndTplot(zb, type="confidence"), tables=TRUE)
  cat(attr(NormalAndTplot(zb), "call"), "\n")

  zr <- z.test(x1+1, stdev=2, alt="less")
  NormalAndTplot(zr)
  print(NormalAndTplot(zr), tables=TRUE)
  print(NormalAndTplot(zr, xlim=c(-3,2)), tables=TRUE)
  print(NormalAndTplot(zr, xlim=c(-3,2), type="confidence"), tables=TRUE)
  print(NormalAndTplot(zr, type="confidence"), tables=TRUE)

  zl <- z.test(x1-1, stdev=2, alt="greater")
  NormalAndTplot(zl)
}
