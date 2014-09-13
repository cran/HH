\name{hhpdf}
\alias{hhpdf}
\alias{hhdev.off}
\alias{hhcapture}
\alias{hhcode}
\title{
R tools for writing HH2: hhpdf, hhdev.off, hhcapture, hhcode
}
\description{
R tools for writing HH2: hhpdf, hhdev.off, hhcapture, hhcode
These functions in the HH package are placeholders used by
the \code{scripts} files.  See details.

}
\usage{
hhpdf(file, ...)

hhdev.off(...)

hhcapture(file, text)

hhcode(file, text)
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
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\details{
  The files in \code{system.file("scripts/hh2", package="HH")}
  contain R code for all examples and figures in the book.
  The examples can all be directly executed by the user.
  The code examples all use these functions.

  The versions of these functions here are essentially placeholders.
  Functions \code{hhpdf} and \code{hhdev.off} are \code{NULL}.
  As a consequence, the code between them will execute and display on
  the default graphics device.
  Function \code{hhcapture} sources its text argument and prints the
  output to the console.
  Function \code{hhcode} prints its text argument to the console.

  While writing the book, these placeholder functions are
  replaced by more elaborate functions
  with the same names that write the graphs onto pdf files and the
  console output to text files.
}
\keyword{misc}
