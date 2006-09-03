"ancova" <-
function(formula, data.in=sys.parent(), ..., x, groups,
                   transpose=FALSE,
                   display.plot.command=FALSE,
                   superpose.level.name="superpose",
                   ignore.groups=FALSE, ignore.groups.name="ignore.groups",
                   blocks, blocks.pch=letters[seq(levels(blocks))],
                   layout, between, main) {
  ## on.exit(browser())
  a.aov <- aov(formula, data=data.in)
  a.aov$call$formula <- substitute(formula)
  a.aov$call$data <- substitute(data.in)

  ## determine sequence:  x+a or x*a  gives c(FALSE,TRUE)
  ##                      a+x or a*x  gives c(TRUE,FALSE)
  ##                      a, x=x      gives c(TRUE)
  ##                      x, groups=a gives c(FALSE)
  tl <- attr(a.aov$terms,"term.labels")
  if (length(tl)==3) tl <- tl[1:2]
  classes <- sapply(data.in[,tl, drop=FALSE], is.factor)
                  ## sapply(tl,
                  ##   function(cc, data) {
                  ##     ccc <- class(data[,cc])
                  ##     if (is.null(ccc)) FALSE
                  ##     else ccc=="factor"},
                  ##   data.in)

  formula.plot <- formula

  if (length(formula[[3]]) == 3) { ## (y ~ x | a) or (y ~ x | a)
    formula.plot[[3]][[1]] <- as.name("|")
    formula.plot[[3]][2:3] <- formula[[3]][2+classes] ## y ~ x | a
    coef.aov <- coef(a.aov)
  }
  else { ## (y ~ a, x=x) or (y ~ x, groups=a)
    formula.plot[[3]] <- (~  x | g)[[2]]
    if (!missing(x)) { ## (y ~ a, x=x)
#      formula.plot[[3]][[2]] <- as.name(deparse(substitute(x))) ## x
      formula.plot[[3]][[2]] <- substitute(x) ## x
      formula.plot[[3]][[3]] <- formula[[3]]                    ## a
      classes <- c(FALSE, classes)
      tl <- c(deparse(substitute(x)), tl)
      coef.aov <- c(coef(a.aov)[1], x=0, coef(a.aov)[-1])
    }
    if (!missing(groups)) { ## y ~ x, groups=a)
      formula.plot[[3]][[2]] <- formula[[3]]                         ## x
#      formula.plot[[3]][[3]] <- as.name(deparse(substitute(groups))) ## a
      formula.plot[[3]][[3]] <- substitute(groups) ## a
      classes <- c(classes, TRUE)
      tl <- c(tl, deparse(substitute(groups)))
      coef.aov <-
        c(coef(a.aov),
          rep(0, length(levels(data.in[[deparse(substitute(groups))]]))-1))
    }
  }
  
  ## xyplot(formula.plot, data=data.in, ...) ## constructed
  m <- match.call()
  m[[1]] <- as.name("xyplot")
  m$formula <- formula.plot
  names(m)[3] <- "data"
  if (length(formula[[3]]) == 1) m$x <- NULL
  m$display.plot.command <- NULL
  m$main <- list(label=deparse(substitute(formula)))
  if (!missing(main)) m$main[names(main)] <- main
  m$coef <- coef.aov
  if (missing(groups)) {
    m$groups <- data.in[[tl[classes]]]
  }
  else {
    m$groups <- data.in[[as.character(m$groups)]]
  }
  m$contrasts <- contrasts(m$groups)
  m$classes <- classes
  m$panel <- "panel.ancova"
  a.labels <- dimnames(m$contrasts)[[1]]

  tpgs <- as.data.frame(trellis.par.get("superpose.symbol"))
  if.R(r=tpgs$col <- as.character(tpgs$col), s={})
  if (is.factor(tpgs$pch)) tpgs$pch <- as.character(tpgs$pch)

  tpgl <- as.data.frame(trellis.par.get("superpose.line"))
  if.R(r=tpgl$col <- as.character(tpgl$col), s={})
  if (is.factor(tpgl$pch)) tpgl$pch <- as.character(tpgl$pch)

##if (missing(blocks))
    m$key <- list(text=list(a.labels),   ## treatment key
                  points=tpgs[1:length(a.labels),],
                  lines=tpgl[1:length(a.labels),],
                  border=TRUE,
                  space="right",
                  title=as.character(formula.plot[[3]][[3]]))
##  else {
##    a.labels <- levels(blocks)
##    m$key <- list(text=list(a.labels),   ## blocks key
##                  points=tpgs[1:length(a.labels),],
##                  lines=tpgl[1:length(a.labels),],
##                  border=TRUE,
##                  space="right",
##                  title=deparse(substitute(blocks)))
##    m$key$points$pch <- blocks.pch[1:length(a.labels)]
##    m$key$points$cex[] <- max(m$key$points$cex)
##  }
  if (!missing(blocks)) {
    blocks.char <- deparse(substitute(blocks))
    bl <- data.in[[blocks.char]]
    if (!is.null(bl)) {
      blocks <- bl
      m$blocks <- blocks
    }
    m$blocks.pch <- blocks.pch
    m$blocks.cex <- m$key$points$cex
    m$key$points <- NULL
    m$sub <- paste(blocks.char, ": ",
                   paste(blocks.pch, collapse=" "),
                   sep="")
  }
  levels.a <- levels(data.in[[as.character(formula.plot[[3]][[3]])]])
  data.in[[as.character(formula.plot[[3]][[3]])]] <-
    factor(data.in[[as.character(formula.plot[[3]][[3]])]],
           levels=c(levels.a, superpose.level.name,
             if(ignore.groups) ignore.groups.name))
## create a groups column that duplicates the classes column
  data.in <- cbind(data.in, new.groups=data.in[[tl[classes]]])
  data2 <- data.in
  data2[[tl[classes]]] <- superpose.level.name
  data3 <- rbind(data.in, data2)
  if (ignore.groups) {
    data2[[tl[classes]]] <- ignore.groups.name
    data3 <- rbind(data3, data2)
  }
  m$data <- data3
  m$groups <- data3$new.groups
  if (missing(layout)) 
    m$layout <- c(length(levels.a)+1, 1)
  else m$layout <- layout
  if (missing(between)) 
    m$between <- list(x=c(rep(0,length(levels.a)-1),2), y=0)
  else m$between <- between
  m$superpose.level.name <- NULL
  
  ## print or evaluate the xyplot call
  if (display.plot.command) print(m)
  m$transpose <- NULL
  if.R(r={names(m)[[match("formula", names(m))]] <- "x"},
       s={})
  if (transpose)
    attr(a.aov,"trellis") <- t(eval(m))
  else
    attr(a.aov,"trellis") <- eval(m)
  oldClass(a.aov) <- c("ancova", oldClass(a.aov))
  a.aov
}

"anova.ancova" <-
function(object, ...)
  NextMethod("anova")

"predict.ancova" <-
function(object, ...)
  NextMethod("predict")

"print.ancova" <-
function(x, ...) {
  print(anova(x, ...))
  print(attr(x,"trellis"))
  invisible(x)
}

"model.frame.ancova" <- function(formula, ...)
  NextMethod("model.frame")

"summary.ancova" <-
function(object, ...)
  NextMethod("summary")

"plot.ancova" <-
function(x, ...) {
  x.full <- x
  attr(x, "trellis") <- NULL
  NextMethod("plot")
  invisible(x.full)
}

"coef.ancova" <-
function(object, ...)
  NextMethod("coef")

"coefficients.ancova" <-
function(object, ...)
  NextMethod("coef")

"panel.ancova" <-
function(x, y, subscripts, groups, transpose=FALSE, ...,
                         coef, contrasts, classes, ignore.groups,
                         blocks, blocks.pch, blocks.cex) {
##  contrasts <- contrasts[-nrow(contrasts), -ncol(contrasts)]
  n.contr <- ncol(contrasts)
  if (length(classes)==1)
    coefs.a <- (1:n.contr) + 1
  else
    coefs.a <- (1:n.contr) + (1:2)[classes]
  a <- coef[1] + contrasts %*% coef[coefs.a]
  
  if (length(classes)==1)
    b <- rep(0, length(a))
  else {
    b <- coef[2+c(0,n.contr)[!classes]]
    if (n.contr != (length(coef)-2))
      b <- b + contrasts %*% coef[-(1:(n.contr+2))]
    else
      b <- rep(b, length(a))
  }
  
  if (transpose) {
    a.untransposed <- a
    a <- ifelse (b==0, 0, -a/b)
    b <- 1/b  ## if (b==0) Inf
  }
  
  tpgs <- as.data.frame(trellis.par.get("superpose.symbol"))
  if.R(r=tpgs$col <- as.character(tpgs$col), s={})
  if (is.factor(tpgs$pch)) tpgs$pch <- as.character(tpgs$pch)

  tpgl <- as.data.frame(trellis.par.get("superpose.line"))
  if.R(r=tpgl$col <- as.character(tpgl$col), s={})
  if (is.factor(tpgl$pch)) tpgl$pch <- as.character(tpgl$pch)

  ## browser()
  cell <- if.R(r={
    sys.frame.i <-
      match(TRUE, sapply(1:length(sys.frames()),
                         FUN=function(i)
                         exists("panel.number", envir=sys.frame(i))))
    get("panel.number", pos=sys.frame(sys.frame.i))},
               s=get("n", frame=sys.parent()))

  if (cell == length(a)+1) {

    if (missing(blocks))
      panel.superpose(x, y, subscripts=subscripts, groups=groups, ...)
    else
      panel.superpose(x, y, subscripts=subscripts, groups=rep(blocks,2),
                        pch=blocks.pch, cex=blocks.cex, ...)
    for (i in seq(along=a)) {
      if (abs(b[i]) == Inf)
        panel.abline(v=a.untransposed[i],
                     col=tpgl[i,"col"], lty=tpgl[i,"lty"], lwd=tpgl[i,"lwd"])
      else
        panel.abline(a=a[i], b=b[i],
                     col=tpgl[i,"col"], lty=tpgl[i,"lty"], lwd=tpgl[i,"lwd"])
    }
  }
  else
    if (cell == length(a)+2) {
      if (missing(blocks))
        panel.superpose(x, y, subscripts=subscripts, groups=groups, ...)
      else
        panel.superpose(x, y, subscripts=subscripts, groups=rep(blocks,3),
                        pch=blocks.pch, cex=blocks.cex, ...)
      if (transpose) {
        tmp.lm <- coef(lm(x ~ y))       # transpose=TRUE interchanged the names
                                        # we must un-transpose here
        ## isolate coefficients
        a <- tmp.lm["(Intercept)"]    
        b <- tmp.lm["y"]
        ## transpose coefficients
        a[] <- ifelse (b==0, 0, -a/b)   # keep "(Intercept)" name
        b <- 1/b  ## if (b==0) Inf
        names(b) <- "x"                 # keep "x" name
      }
      else {
        tmp.lm <- coef(lm(y ~ x))
        a <- tmp.lm["(Intercept)"]
        b <- tmp.lm["x"]
      }
      if (transpose && tmp.lm["y"]==0)
        panel.abline(v=tmp.lm["(Intercept)"])
      else
        panel.abline(a, b)
    }
    else {
      if (missing(blocks))
          panel.xyplot(x, y, col=tpgl[cell,"col"], pch=tpgs[cell,"pch"], ...)
        else
          panel.superpose(x, y, subscripts=subscripts, groups=blocks,
                        pch=blocks.pch, cex=blocks.cex, ...)
      if (abs(b[cell]) == Inf)
        panel.abline(v=a.untransposed[cell],
                     col=tpgl[cell,"col"],
                     lty=tpgl[cell,"lty"],
                     lwd=tpgl[cell,"lwd"])
      else
        panel.abline(a=a[cell], b=b[cell],
                     col=tpgl[cell,"col"],
                     lty=tpgl[cell,"lty"],
                     lwd=tpgl[cell,"lwd"])
    }
}

