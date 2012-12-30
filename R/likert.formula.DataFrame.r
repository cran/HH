if (FALSE) {
data(NZScienceTeaching)
NZ <- NZScienceTeaching
names(NZ) <- NULL
NZ <- do.call(rbind, NZ)
NZ <- data.frame(NZ, check.names=FALSE,
            Subtable=rep(
              attr(NZScienceTeaching,"names"),
              sapply(NZScienceTeaching, nrow)))
class(NZ) <- c("likertDataFrame", class(NZ))
names(NZ)
NZ

likert(Question ~ . | Subtable, data=NZScienceTeaching)

}

## as.listOfNamedMatrices.likertDataFrame <-
##   function(x,
##            xName=deparse(substitute(x)),
##            SubtableColumn=which(sapply(x, class) == "factor"),
##            ...) {
##     if (is.character(SubtableColumn))
##       SubtableColumn <- which(names(x) == SubtableColumn)
##     if (inherits(data, "likertDataFrame"))
##       class(data) <- class(data)[class(data) != "likertDataFrame"]
##     x.split <- split(x[, -SubtableColumn], x[, SubtableColumn])
##     as.listOfNamedMatrices(x.split)
##   }

## ## don't need this one.
## print.likertDataFrame <- function(x, SubtableColumn="Subtable", ...) {
##   print(as.listOfNamedMatrices(x, SubtableColumn=SubtableColumn, ...))
##   invisible(x)
## }
##
## If I later decide to bring it back, put the line
## S3method(print, likertDataFrame)
## in the NAMESPACE file.

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
data(NZScienceTeaching)
as.likertDataFrame(NZScienceTeaching)

data(ProfChal)
as.likertDataFrame(ProfChal)

data(SFF8121)
as.likertDataFrame(SFF8121)
}



plot.likert.formula <- function(x, data, ..., xName=deparse(sys.call()), main) {
  force(xName)
  if (missing(data)) stop("The data argument is required.", call.=FALSE)
  if (missing(main)) {
    main <- match.call()[1:3]
    names(main)[] <- ""
    main[[1]] <- as.name("likert")
    main <- deparse(main)
  }

  if (length(x) < 3)
    stop(paste("formula must be in form\n  Question ~ . | Subtable\nor\n  Question ~ A+B+C | Subtable"), call.=FALSE)

  QuestionName <- as.character(x[[2]])

  ## convert non-'.' RHS to '.', and subset data.frame to match
  if ((x[[3]] != ".") && ((length(x[[3]]) != 3) || (x[[3]][[2]] != "."))) {
    DOT <- as.call(~ .)[[2]]
    if ((length(x[[3]]) > 1) && (x[[3]][[1]] == "|")) {
      CondName <- as.character(x[[3]][[3]])
      LevelNamesFormula <- x[[3]][[2]]
      x[[3]][[2]] <- DOT
    }
    else {
      CondName <- NULL
      LevelNamesFormula <- x[[3]]
      x[[3]] <- DOT
    }

    LevelNamesRaw <- strsplit(deparse(LevelNamesFormula), "+", fixed=TRUE)[[1]]
    LevelNames <- sub('^[[:space:]]+', '', sub('[[:space:]]+$', '', LevelNamesRaw )) ## remove leading and trailing white space, POSIX-style
    data <- data[c(QuestionName, CondName, LevelNames)]
  }

  ## the assignment of rownames can't be factored out here.
  ## The listOfNamedMatrices could have identical rownames, and then data.frame() would complain.
  if (x[[3]] == ".") {
    rownames(data) <- data[, QuestionName]
    names.dimnames <- attr(data, "names.dimnames")
    data[, QuestionName] <- NULL
    data <- data.matrix(data[sapply(data, is.numeric)]) ## not redundant, data.matrix converts character columns to NA
    names(dimnames(data)) <- names.dimnames
    plot.likert.default(data, xName=xName, ..., main=main)
  }
  else  ## if (x[[3]][[2]] == ".") ## implied
    {
      SubtableName <- as.character(x[[3]][[3]])
      data.rownames <- split(data[, QuestionName], data[, SubtableName])
      data[, QuestionName] <- NULL

      SubtableColumn <- which(names(data) == SubtableName)
      x.split <- split(data[, -SubtableColumn], data[, SubtableName])
      names.dimnames <- attr(data, "names.dimnames")
      data <- as.listOfNamedMatrices(x.split)

      data <- mapply(`rownames<-`, data, data.rownames, SIMPLIFY=FALSE)
      data <- lapply(data, data.matrix)
      ## data <- lapply(data, `attr<-`, "names.dimnames", names.dimnames)
      data <- lapply(data, function(z) {
        names(dimnames(z)) <- names.dimnames
        z
      })
      plot.likert.list(data, xName=xName, ..., main=main)
    }

}


if (FALSE) {
data(NZScienceTeaching)

likert(Question ~ . | Subtable,
               data=as.likertDataFrame(NZScienceTeaching))

likert(Question ~ ., as.likertDataFrame(NZScienceTeaching))


data(ProfChal)
likert(Question ~ . | Subtable, as.likertDataFrame(ProfChal))
likert(Question ~ ., as.likertDataFrame(ProfChal))
likert(Question ~ . | Subtable, as.likertDataFrame(ProfChal), as.percent=TRUE)


data(SFF8121)
likert(Question ~ . | Subtable, as.likertDataFrame(SFF8121), layout=c(2,1))
}
