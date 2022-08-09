latex.array <-
  function (object, ..., var.sep = "}\\tabularnewline{\\bfseries ", value.sep = ": ",
            use.ndn = TRUE, cgroup = NULL, rgroup = NULL, n.rgroup = NULL,
            ## rgroup here captures and ignores any incoming rgroup argument
            title = first.word(deparse(substitute(object))), rowlabel = title,
            rsubgroup=NULL, n.rsubgroup=NULL
            )
{


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

    if (is.null(rsubgroup) & is.null(n.rsubgroup)
        ) {
      n.rgroup <- rep(dimx[1], length(klmxyz))
      rgroup <- klmxyz
      mclength <- dim(tmp5)[[2]]+1
      rgroup <- paste0("}\\vspace*{-1.75ex}\\tabularnewline\\multicolumn{",mclength,"}{l}{\\bfseries ",
                         rgroup, "}\\tabularnewline\\vspace*{-2.5ex}\n{")
      latex(tmp5, rgroup = rgroup, n.rgroup = n.rgroup,
            title = title, rowlabel = rowlabel, cgroup = cgroup,
            ...)
      }
    else {
      ## Rgroup
      n.rgroup <- rep(dimx[1], length(klmxyz))
      rgroup <- klmxyz

      n.Rgroup <- rep(n.rsubgroup, length(n.rgroup))
      Rgroup <- matrix(rsubgroup, nrow=length(rsubgroup), ncol=length(n.rgroup))
      ## Rgroup[1,] <- paste("",rgroup, Rgroup[1,], sep="\\tabularnewline\\bf ")
      mclength <- dim(tmp5)[[2]]+1
      Rgroup[] <- paste0("}\\vspace*{-1.75ex}\\tabularnewline\\multicolumn{",mclength,"}{l}{\\bfseries ",
                         Rgroup, "}\\tabularnewline\\vspace*{-2.5ex}\n{")
      Rgroup[1,] <- paste0("}\\vspace*{.5ex}\\tabularnewline\n",
                           "\\multicolumn{",
                           mclength,
                           "}{l}{\\bfseries ",
                           rgroup,
                           "}\\vspace*{-1ex}\\tabularnewline\n{\\bfseries ",
                           Rgroup[1,])

      latex(tmp5, rgroup = c(Rgroup), n.rgroup = n.Rgroup,
            title = title, rowlabel = rowlabel, cgroup = cgroup,
            ...)
    }
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

## must spell pdf.latex in full because pdf is not a generic function.
## pdf.latex is needed for the file argument, as Hmisc::dvi does not take the file argument
pdf.latex <- function(latex.object, ..., file,
                      overwrite=TRUE, copy.mode=TRUE, copy.date=TRUE) {
  dvi.object <- Hmisc::dvi(latex.object, ...)
  if (missing(file))
    file <- paste0(strsplit(basename(latex.object$file), split=".", fixed=TRUE)[[1]][1], ".pdf")
  copyresult <- file.copy(from=dvi.object$file, to=file,
                          overwrite=overwrite, copy.mode=copy.mode, copy.date=copy.date)
  if (copyresult) invisible(file) else copyresult
}
