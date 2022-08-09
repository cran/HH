\name{pdf.latex}
\alias{pdf.latex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a pdf file from a "latex" file.  See Hmisc::latex for concepts.
}
\description{
Construct a \code{"pdf"} file from a \code{"latex"} file.  See \code{\link[Hmisc]{latex}} for concepts.
}
\usage{
pdf.latex(latex.object, ..., file, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latex.object}{
Result from a call to \code{Hmisc::latex()}.
}
  \item{\dots}{
Optional arguments to \code{Hmisc::dvi()}
}
  \item{file}{
File name in \code{getwd()} to place resulting pdf file.
}
  \item{overwrite}{
If the file already exists, \code{TRUE} means replace it.
}
  \item{copy.mode, copy.date}{
If \code{TRUE} copy file mode and date from temporary directory to \code{getwd()}.
}
}
\value{
Filename of class \code{"dvi"}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link[Hmisc]{latex}}
}
\examples{
## you will normally need these options.  See ?Hmisc::latex for details.
options(latexcmd='pdflatex')
options(dviExtension='pdf')
options(xdvicmd='open')  ## Macintosh, Windows,      SMP linux

\dontrun{
## these examples place files in your current working directory

## matrix
tmp <- array(1:20, c(4,5), list(LETTERS[1:4], LETTERS[5:9]))
tmp

pdf.latex(latex(tmp)) ## for matrix, accept the default structure.tex and structure.pdf filenames.

pdf.latex(latex(tmp, title="tmp")) ## specify name of .tex and .pdf file.



## 3D array
tmp3 <- array(1:40, c(4,5,2), list(LETTERS[1:4], LETTERS[5:9], LETTERS[10:11]))
tmp3

pdf.latex(latex(tmp3)) ## for array, the default base filename is the
                       ##  name of the argument, hence tmp3.tex and tmp3.pdf

pdf.latex(latex(tmp3, title="somethingelse")) ## or specify somethingelse
}

}
