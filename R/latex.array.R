latex.array <-
function(object, ...,
         var.sep="}\\\\{\\bfseries ",  ## " ; ",
         value.sep=": ",
         use.ndn=TRUE, cgroup=NULL,
         rgroup=NULL, n.rgroup=NULL,
         ## rgroup here captures and ignores any incoming rgroup argument
         title=first.word(deparse(substitute(object))),
         rowlabel=title) {


  dimx <- dim(object)
  dnx <- dimnames(object)
  ndnx <- names(dnx)

  if (use.ndn & missing(rowlabel) & !is.null(ndnx[1])) rowlabel <- ndnx[1]

  if (use.ndn & missing(cgroup)) cgroup <- ndnx[2]

  if (length(dnx) <= 2) ## you should never get here as latex.matrix will get dispatched directly
    return(latex(object, rgroup=rgroup, n.rgroup=n.rgroup,
                 title=title, rowlabel=rowlabel, cgroup=cgroup, ...))

  klmxyz <- if (use.ndn && !is.null(ndnx[3]))
              paste0(ndnx[3], value.sep, dnx[[3]])
            else dnx[[3]]
  if (length(dnx) > 3) {
    for (i in 4:length(dnx))
      klmxyz <- outer(klmxyz,
                      if (use.ndn && !is.null(ndnx[i]))
                        paste0(ndnx[i], value.sep, dnx[[i]])
                      else
                        dnx[[i]],
                      paste,  ## outer.paste()
                      sep=var.sep)
  }

  tmp2 <- object
  dim(tmp2) <- c(dimx[1:2], prod(dimx[3:length(dimx)]))
  dimnames(tmp2) <- c(dnx[1:2], list(c(klmxyz)))
  tmp2

  tmp3 <- aperm(tmp2, c(2,1,3))
  tmp3

  tmp4 <- tmp3
  dim(tmp4) <- c(dim(object)[2], prod(dim(object)[-2]))
  tmp4

  tmp5 <- t(tmp4)
  tmp5
  dimnames(tmp5) <- list(rep(dimnames(object)[[1]],
                             prod(dim(object)[-(1:2)])),
                         dimnames(object)[[2]])
  tmp5

  if (!is.null(rgroup) || !is.null(n.rgroup))
    warning("Input rgroup and n.rgroup are ignored")

  ## this now goes to the latex.matrix function which changes
  ## class to default and thus prevents cycling.
  latex(tmp5, rgroup=klmxyz, n.rgroup=rep(dimx[1], length(klmxyz)),
        title=title, rowlabel=rowlabel, cgroup=cgroup, ...)
}

## latex.matrix is here to catch 2-dimensional
## c("matrix","array") objects and keep them from cycling
latex.matrix <- function(object, ...,
                         use.ndn=TRUE, cgroup=NULL,
                         title=first.word(deparse(substitute(object))),
                         rowlabel=title) {
  dimx <- dim(object)
  dnx <- dimnames(object)
  ndnx <- names(dnx)

  if (use.ndn & missing(rowlabel) & !is.null(ndnx[1])) rowlabel <- ndnx[1]

  if (use.ndn & missing(cgroup)) cgroup <- ndnx[2]

  class(object) <- "default"
  latex(object,
        title=title, rowlabel=rowlabel, cgroup=cgroup, ...)
}

latex.table <- function(object, ...) {
    class(object) <- c("matrix", "array", class(object))
    latex.array(object, ...)
}
