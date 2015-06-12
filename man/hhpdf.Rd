\name{hhpdf}
\alias{hhpdf}
\alias{hhdev.off}
\alias{hhcapture}
\alias{hhcode}
\alias{hhpng}
\alias{hhlatex}
\title{
R tools for writing HH2: hhpdf, hhdev.off, hhcapture, hhcode, hhpng, hhlatex
}
\description{
R tools for writing HH2: hhpdf, hhdev.off, hhcapture, hhcode, hhpng, hhlatex.
These functions in the HH package are placeholders used by
the \code{scripts} files.  See details.

}
\usage{
hhpdf(file, ...)

hhdev.off(...)

hhcapture(file, text, echo=TRUE, print.eval=TRUE)

hhcode(file, text)

hhpng(file, ...)

hhlatex(file="", ...)
}
\arguments{
  \item{file}{
    Output file name.  Ignored.
  }
  \item{text}{
    Multi-line character string.  It will be
    displayed on the console by \code{hhcode}, and will be executed
    and the resulting value displayed on the console by
    \code{hhcapture}.
  }
  \item{\dots}{
    Ignored.
  }
  \item{echo, print.eval}{
    See \code{\link[base]{source}}.
  }
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\details{
  The files in \code{\link{HHscriptnames}}\code{()}
  contain R code for all examples and figures in the book.
  The examples can all be directly executed by the user.
  The code examples all use these functions.

  The versions of these functions here are essentially placeholders.
  Functions \code{hhpdf}, \code{hhpng}, and \code{hhdev.off} are no-ops and return \code{NULL}.
  As a consequence, the code between them will execute and display on
  the default graphics device.
  Function \code{hhcapture} sources its text argument and prints the
  output to the console.
  Function \code{hhcode} prints its text argument to the console.
  Function \code{hhlatex} prints the latex source to the
  console and returns \code{NULL}.

  While writing the book, these placeholder functions are
  replaced by more elaborate functions
  with the same names that write the graphs onto pdf or png files, the
  console output to text files, and the latex code to a file.
}
\keyword{misc}
