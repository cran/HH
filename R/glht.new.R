if.R(s={},
     r={
## in preparation for HH_2.1.24 or for next release of multcomp
## based on multcomp_1.0-7

mcp2matrix2 <-
function (model, linfct, `interaction_average`=FALSE, `covariate_average`=FALSE) 
{
    fc <- `factor_contrasts`(model)
    contrasts <- fc$contrasts
    factors <- fc$factors
    intercept <- fc$intercept
    mf <- fc$mf
    mm <- fc$mm
    alternative <- NULL
    if (!is.list(linfct) || is.null(names(linfct))) 
        stop(sQuote("linfct"), "is not a named list")
    nhypo <- names(linfct)
    checknm <- nhypo %in% rownames(factors)
    if (!all(checknm)) 
        stop("Variable(s) ", sQuote(nhypo[!checknm]), " have been specified in ", 
            sQuote("linfct"), " but cannot be found in ", sQuote("model"), 
            "! ")
    if (any(checknm)) {
        checknm <- sapply(mf[nhypo[checknm]], is.factor)
        if (!all(checknm)) 
            stop("Variable(s) ", sQuote(paste(nhypo[!checknm], 
                collapse = ", ")), " of class ", sQuote(paste(sapply(mf[nhypo[!checknm]], 
                class), collapse = ", ")), " is/are not contained as a factor in ", 
                sQuote("model"), ".")
    }
    m <- c()
    ctype <- c()
    for (nm in nhypo) {
        if (is.character(linfct[[nm]])) {
            Kchr <- function(kch) {
                types <- eval(formals(contrMat)$type)
                pm <- pmatch(kch, types)
                if (!is.na(pm)) {
                  tmpK <- contrMat(table(mf[[nm]]), type = types[pm])
                  ctype <<- c(ctype, types[pm])
                }
                else {
                  tmp <- chrlinfct2matrix(kch, levels(mf[[nm]]))
                  tmpK <- tmp$K
                  m <<- c(m, tmp$m)
                  alternative <<- tmp$alternative
                }
                if (is.null(rownames(tmpK))) 
                  rownames(tmpK) <- paste(kch, 1:nrow(tmpK), 
                    sep = "_")
                if (length(nhypo) > 1) 
                  rownames(tmpK) <- paste(nm, rownames(tmpK), 
                    sep = ": ")
                list(K = tmpK)
            }
            tmp <- lapply(linfct[[nm]], Kchr)
            linfct[[nm]] <- do.call("rbind", lapply(tmp, function(x) x$K))
        }
    }
    hypo <- vector(mode = "list", length = length(nhypo))
    names(hypo) <- nhypo
    for (nm in nhypo) {
        if (is.character(contrasts[[nm]])) {
            C <- do.call(contrasts[[nm]], list(n = nlevels(mf[[nm]])))
        }
        else {
            C <- contrasts[[nm]]
        }
        if (intercept) {
            Kstar <- linfct[[nm]] %*% C
        }
        else {
            Kstar <- linfct[[nm]]
        }
        pos <- factors[nm, ] == 1
        cov.ave <- ((length(`covariate_average`) == 1 &&
                     (`covariate_average`=="mean" || `covariate_average`==TRUE)) ||
                    (length(`covariate_average`) > 1))
        if (sum(pos) > 1) {
          if (!`interaction_average`) {
            warning("covariate interactions found -- ", "default contrast might be inappropriate")
            classes <- character(0)
          }
          else
            {##recover()
             Kinter <- c()
            for (i in which(pos)[-1]) {
##             for (i in which(pos[apply(factors, 2, sum)==1])) { ## rmh []
               k <- sum(attr(mm, "assign") == i)/ncol(Kstar)
               ivar <- rownames(factors)[factors[, i] == 1]
               ivar <- ivar[ivar != nm]
               classes <- sapply(mf[, ivar, drop = FALSE], is.factor)
               if (all(classes)) {
                 fact <- 1/(k + 1)
               }
               else {
                 fact <- 1
                 cov.ave <- cov.ave ||
                 !((length(`covariate_average`)==1) &&
                   (`covariate_average` == FALSE))
                 if (!cov.ave)
                   warning("covariate interactions found -- please choose appropriate contrast")
               }
               if (sum(factors[1:which(rownames(factors) == 
                                       nm), i]) == 1) {
                 Kinter <- cbind(Kinter, Kstar[, rep(1:ncol(Kstar), 
                                                     k), drop = FALSE] * fact)
               }
               else {
                 Kinter <- cbind(Kinter, Kstar[, rep(1:ncol(Kstar), 
                                                     rep(k, ncol(Kstar))), drop = FALSE] * fact)
               }
             }
             Kstar <- cbind(Kstar, Kinter)
           }
        }
        else
          classes <- character(0)
        hypo[[nm]] <- list(K = Kstar, where = attr(mm, "assign") %in% 
                           which(factors[nm, ] == 1))
      }
    Ktotal <- matrix(0, nrow = sum(sapply(hypo, function(x) nrow(x$K))), 
        ncol = ncol(mm))
    colnames(Ktotal) <- colnames(mm)
    count <- 1
    for (h in hypo) {
        Ktotal[count:(count + nrow(h$K) - 1), h$where] <- h$K
        count <- count + nrow(h$K)
    }
    if (!is.matrix(Ktotal)) 
        Ktotal <- matrix(Ktotal, nrow = 1)
    rownames(Ktotal) <- unlist(lapply(hypo, function(x) rownames(x$K)))
    if (is.null(ctype)) 
        ctype <- "User-defined"
    ctype <- paste(unique(ctype), collapse = ", ")
    attr(Ktotal, "type") <- ctype
    if (length(m) == 0) 
        m <- 0
    if (!all(classes) && cov.ave) {
      ## recover()
      switch(as.character(length(classes)),
             "0"={},
             "1"= {if (length(`covariate_average`) != 1)
                     stop("inconsistent `covariate_average`")
                   c.a <-
                     if (`covariate_average`=="mean" || `covariate_average`==TRUE)
                       mean(mf[, ivar, drop = FALSE])
                     else
                       unlist(`covariate_average`)
                   whichCols <- grep(names(classes), dimnames(Ktotal)[[2]])
                   Ktotal[,whichCols] <- Ktotal[,whichCols] * c.a
                 },
             { ## 2 or more
           stop("not yet written, similar to 1.")    
             })
      }
    list(K = Ktotal, m = m, alternative = alternative, type = ctype)
  }
environment(mcp2matrix2) <- environment(glht)

glht.mcp <-
  function (model, linfct, ..., `interaction_average`=FALSE, `covariate_average`=FALSE)
{
  tmp <- mcp2matrix2(model, linfct = linfct,
                    `interaction_average`=`interaction_average`,
                    `covariate_average`=`covariate_average`)
  args <- list(model = model, linfct = tmp$K)
  if (!is.null(tmp$alternative)) 
    args$alternative <- tmp$alternative
  if (any(tmp$m != 0)) 
    args$rhs <- tmp$m
  args <- c(args, list(...))
  ret <- do.call("glht", args)
  ret$type <- tmp$type
  ret$focus <- names(linfct)
  return(ret)
}
environment(glht.mcp) <- environment(glht)
})

## source("~/HH-R.package/HH/R/glht.new.R")

