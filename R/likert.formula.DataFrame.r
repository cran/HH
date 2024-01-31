as.likertDataFrame.listOfNamedMatrices <-
  function(x, xName=deparse(substitute(x))) {
    names.x <- names(x)
    names(x) <- NULL
    rownames.x <- unname(unlist(lapply(x, rownames)))
    xNULL <- lapply(x, `rownames<-`, NULL)
    xx <- do.call(rbind, xNULL)
    xx <- data.frame(xx,
                     Question=factor(rownames.x, levels=unique(rownames.x)),
                     Subtable=rep(
                       factor(names.x, levels=names.x),
                       sapply(xNULL, nrow)),
                     check.names=FALSE)
    attr(xx,"names.dimnames") <- names(dimnames(x[[1]]))
    xx
  }



as.likertDataFrame <- function(x, xName=deparse(substitute(x)))
  UseMethod("as.likertDataFrame")

as.likertDataFrame.array <- function(x, xName=deparse(substitute(x)))
  as.likertDataFrame(as.listOfNamedMatrices(x, xName=xName))

as.likertDataFrame.matrix <- function(x, xName=deparse(substitute(x))) {
  result <- data.frame(x, Question=rownames(x), check.names=FALSE)
  attr(result, "names.dimnames") <- names(dimnames(x))
  result
}

as.likertDataFrame.data.frame <- as.likertDataFrame.matrix




if (FALSE) {
tmp <- data.frame(a=c("hundred","thousand","million"),
                  b=factor(c("hundred","thousand","million")),
                  c=factor(c("hundred","thousand","million"), levels=c("hundred","thousand","million")),
                  d=1:3,
                  stringsAsFactors=FALSE)
sapply(tmp, class)
sapply(tmp, levels)

split(tmp$d, tmp$a) ## split coerces a to an alphabetical factor
split(tmp$d, tmp$b) ## b is already an alphabetical factor
split(tmp$d, tmp$c) ## c uses the assigned factor order

likert(a ~ d, data=tmp)
likert(b ~ d, data=tmp)
likert(c ~ d, data=tmp)


tmp2 <- cbind(rbind(tmp, tmp),
              g=rep(c("e","d"), each=3),
              h=factor(rep(c("e","d"), each=3)),
              i=factor(rep(c("e","d"), each=3), levels=c("e","d")))
sapply(tmp2, levels)

likert(c ~ d | g, data=tmp2)
likert(c ~ d | h, data=tmp2)
likert(c ~ d | i, data=tmp2)
}




## if (FALSE) {
## data(NZScienceTeaching)

## likert(Question ~ ., NZScienceTeaching)

## likert(Question ~ . | Subtable, NZScienceTeaching, layout=c(1,2), scales=list(y=list(relation="free")))


## data(ProfChal)
## ## these are the twelve possible structures for formulas
## likert(Question ~ .                          , ProfChal)
## likert(Question ~ .                | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))
## likert(Question ~ Agree                      , ProfChal)
## likert(Question ~ Agree            | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))
## likert(Question ~ Disagree + Agree           , ProfChal)
## likert(Question ~ Disagree + Agree | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))
## likert(         ~ .                          , ProfChal)
## likert(         ~ .                | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))
## likert(         ~ Agree                      , ProfChal)
## likert(         ~ Agree            | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))
## likert(         ~ Disagree + Agree           , ProfChal)
## likert(         ~ Disagree + Agree | Subtable, ProfChal, layout=c(1,6), scales=list(y=list(relation="free")))

## likert(Question ~ . | Subtable, ProfChal, as.percent=TRUE, layout=c(1,6), scales=list(y=list(relation="free")))


## data(SFF8121)
## likert(Question ~ . | Subtable, as.likertDataFrame(SFF8121), layout=c(2,1))
## }
