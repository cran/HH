\name{latex.array}
\alias{latex.array}
\alias{latex.matrix}
\alias{latex.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate the latex code for an \code{"array"} or \code{"table"} with 3, 4, or more dimensions.
}
\description{
Generate the latex code for an \code{"array"} or \code{"table"} with 3, 4, or more dimensions.
}
\usage{
\method{latex}{array}(object, ...,
       var.sep = "}\\\\tabularnewline{\\\\bfseries ", value.sep = ": ",
       use.ndn = TRUE, cgroup = NULL,
       ## rgroup here captures and ignores any incoming rgroup argument
       rgroup = NULL, n.rgroup = NULL,
       title = first.word(deparse(substitute(object))),
       rowlabel=title,
            rsubgroup=NULL, n.rsubgroup=NULL)

\method{latex}{matrix}(object, ...,
        use.ndn=TRUE, cgroup=NULL,
        title=first.word(deparse(substitute(object))),
        rowlabel=title)

\method{latex}{table}(object, ...) ## prepend c("matrix", "array") to the
        ## class of the input object, and then call latex.default

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A \code{c("matrix", "array")} or \code{"table"} object.
  }
  \item{\dots}{
    Arguments forwarded to the "default" method for \code{\link[Hmisc]{latex}}.
  }
  \item{use.ndn}{Logical.
    \code{ndn} is an abbreviation for "Names of DimNames".  When
    \code{TRUE} (the default), the \code{rowlabel}, \code{cgroup}, and
    \code{rgroup} values will be taken from the
    \code{names(dimnames(object))}.
  }
  \item{rgroup, n.rgroup}{
    These are the standard arguments for \code{\link[Hmisc]{latex}}.
    Incoming values for \code{rgroup} and \code{n.rgroup} are ignored by \code{latex.array}
    and replaced with values constructed from the names of the dimnames of the third
    and higher dimensions of the input array \code{object}.
    Each item in \code{rgroup} is assigned the appropriate
    combination of \code{names(dimnames(object))[-(1:2)]}.}
  \item{rsubgroup, n.rsubgroup}{
    These are based on the standard arguments for \code{\link[Hmisc]{latex}}.
    Incoming values for \code{rsubgroup} and \code{n.rsubgroup} are 
    applied to the rows of each \code{rgroup}.}
  \item{title, rowlabel, cgroup}{
    These are the standard arguments for \code{\link[Hmisc]{latex}}.
    When \code{use.ndn} is \code{TRUE} (the default), then
    \code{rowlabel} is assigned the \code{names(dimnames(object))[1]} and
    \code{cgroup} is assigned the \code{names(dimnames(object))[2]}.
    }
  \item{value.sep}{When \code{use.ndn} is \code{TRUE} (the default), and
    \code{length(dim(object)) >= 3} then this string is used in the
    constructed \code{rgroup} values
    to separate the factor name from the factor level of the specified
    dimension, for example \code{ABC: 5}.
  }
  \item{var.sep}{When \code{use.ndn} is \code{TRUE} (the default), and
    \code{length(dim(object)) >= 4} then this string is used in the
    in the constructed \code{rgroup} values
    to separate the name and level of each dimension, for example
    \code{ABC: 5 ; DEF: 6}.  The default value is exactly what
    \code{Hmisc::latex}
    needs in order to place two or more lines (one for each dimension) in boldface.
  }
}
\value{
See \code{\link[Hmisc]{latex}}.
}
\details{
  \code{latex.matrix} calls \code{latex.default} directly.
  When \code{use.ndn} is \code{TRUE} (the default), \code{rowlabel} and
  \code{cgroup} are constructed from \code{names(dimnames(object))}
  unless the user explicitly specified them.

  \code{latex.array} appends all two-dimensional layers
  \code{object[,, one, at, a, time]} into a single long \code{"matrix"},
  ignores any incoming \code{rgroup} and \code{n.rgroup} (with a warning),
  and constructs \code{rgroup} and \code{n.rgroup} to label the layers.
  When \code{use.ndn} is \code{TRUE} (the default), \code{rowlabel} and
  \code{cgroup} are constructed from \code{names(dimnames(object))}
  unless the user explicitly specified them.

  \code{latex.table} prepends \code{c("matrix", "array")} to the class of
  the \code{"table"} object, then calls the generic \code{"latex"}.
  This step is necessary because the \code{survey} package creates
  objects whose class includes the value \code{"table"} but not the values
  \code{c("matrix", "array")}.  Should this object be sent directly to
  \code{latex.default}, it would cause on error for any table with
  dimension larger than two.
}

\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Hmisc]{latex}}
}
\examples{

\dontrun{
## These are the recommended options.  See ?Hmisc::latex for details.
options(latexcmd='pdflatex')
options(dviExtension='pdf')
options(xdvicmd='open')  ## Macintosh, Windows,      SMP linux
}

## This sets up the defaults for latex to write to a pdf file
microplot::latexSetOptions()
## It is needed for R CMD check.
## It is recommended if you normally use pdflatex.
## If you want some other destination for latex, use a non-default argument.


  tmp3 <- array(1:8, c(2,2,2),
                list(letters[1:2],
                     letters[3:4],
                     letters[5:6]))

  tmp3

  ltmp3 <- latex(tmp3) ## assignment prevents display of the generated pdf file
                       ## enter the object name to display the file on screen
  ## ltmp3

  ## latex(tmp3) causes a file tmp3.tex to be created in the working directory.
  ## A user might want to keep tmp3.tex and \input{tmp3.tex} it into a longer .tex file.
  ## R CMD check doesn't like tmp3.tex to remain, so it is removed here.
  file.remove("tmp3.tex")

\dontrun{
  try(  ## warning: Input rgroup and n.rgroup are ignored
    latex(tmp3, rgroup=letters[1:3], n.rgroup=c(1,1,2), file="ignorergroup.tex")
     )

  names(dimnames(tmp3)) <- LETTERS[24:26]
  latex(tmp3, file="LETTERS3.tex")
  latex(tmp3, rowlabel="Something Else", file="SomethingElse.tex")


  tmp4 <- array(1:120, c(5,4,3,2),
                list(letters[1:5],
                     letters[6:9],
                     letters[10:12],
                     letters[13:14]))

  tmp4
  latex(tmp4, var.sep=" ; ")

  names(dimnames(tmp4)) <- LETTERS[23:26]
  latex(tmp4, file="LETTERS4.tex")


  ## with rsubgroup and n.rsubgroup
  latex(tmp4, var.sep=" ; ", file="LETTERS4sub.tex",
        rsubgroup=c("Three","Two"), n.rsubgroup=c(3,2))

  ## with rsubgroup and n.rsubgroup and cgroup and n.cgroup
  latex(tmp4, var.sep=" ; ", file="LETTERS4sub.tex",
        rsubgroup=c("Three","Two"), n.rsubgroup=c(3,2),
        cgroup=c("FGH","I"), n.cgroup=c(3,1))


  tmp2 <- array(1:6, c(3,2),
                list(Rows=letters[1:3],
                     Columns=letters[4:5]))

  tmp2

  latex(tmp2)

  ## Input rgroup honored for "matrix"
  latex(tmp2, rgroup=c("Two","One"), n.rgroup=c(2,1), file="rgroup.tex")

  latex(tmp2, rowlabel="something else", file="something.tex")

  ## tableDemo is based on a table constructed from
  ##         survey::svytable(~ FactorA + FactorB + FactorC, Survey.Design.Object)
  tableDemo <- structure(c(28, 25, 33, 12, 6, 22,  8, 12, 23, 24,  6, 32,
                           32, 31, 59, 11, 2, 33, 10,  3, 23,  7,  2, 26),
                         .Dim = c(3L, 4L, 2L),
                         .Dimnames = list(FactorA = c("a", "b", "c"),
                                          FactorB = c("d", "e", "f", "g"),
                                          FactorC = c("h", "i")),
                         class = "table")

  class(tableDemo)
  latex(tableDemo)
}

}
