\name{hh}
\alias{hh}
\alias{hh.old}
\alias{hh.file}
\alias{hh.file.DOS}
\alias{HH.ROOT.DIR}
\alias{HHfile.ROOT.DIR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Resolve filenames relative to the HH directory. }
\description{
  The pathnames in the HH package for all the datasets referenced in
  Heiberger and Holland (2004) are given relative to
  the \code{options()$HH.ROOT.DIR} directory.
  The pathnames for all the executable files in the online
  files accompanying Heiberger and Holland (2004) are given relative to
  the\cr
  \code{options()$HHfile.ROOT.DIR} directory.
}
\usage{
hh(file)     ## gives message about change in usage beginning with HH_2.3-17
hh.old(file) ## new name for function hh() prior to HH_2.3-17
hh.file(file)
hh.file.DOS(file, displayForCutAndPaste=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Character string giving the pathname of a file.

    For \code{hh}, the file is in the HH package and the pathname is
    relative to\cr
    \code{options()$HH.ROOT.DIR}, the installed
location of the HH package.  The option is set automatically when the HH package is loaded by
\code{library} or \code{require}.  This function is normally used to
access
the datasets that come with the HH book and package.

For \code{hh.file} and \code{hh.file.DOS}, the file is in the HH online
files which need to be independently downloaded from the HH book's
website \url{http://springeronline.com/0-387-40270-5}.
The pathname is relative to \code{options()$HHfile.ROOT.DIR},
which must be set by the user to correspond to the location where the HH
online files are stored on the specific computer.  The \code{file} argument to these
two functions is the relative pathname exactly as specified in the
captions of figures and tables in Heiberger and Holland (2004).
}

\item{displayForCutAndPaste}{Logical value.  When \code{TRUE} (the
default) the function \code{hh.file.DOS} prints the full pathname with
the \samp{"\\"} file separator convention so it can be picked up and
pasted into an editor that
uses the MS DOS convention for file paths.  The function
\code{hh.file.DOS} always returns a value
with the full pathname using the \samp{"\\\\"} convention so it could be
used as an argument to an R or S-Plus function.
}
}
\details{
Beginning with HH_2.3-17, access to datasets with the notation
\code{hh("abcde <- datasets/abcde.dat")} is defunct.  Instead, use the
notation \code{data(abcde)}.  The old notation will generate an error
with a message to use the new notation.

The files accessed with the notation
\code{hh("chaptername/code/normpdf.r")} are defunct.  Instead use the
new files accessed with the notation
\code{hh.old("scripts/Chxx-chaptername.r")}.  The old notation will
generate an error with a message to use the new notation.

The datasets from the Heiberger and Holland (2004) online files are
all given paths relative to the beginning of the \code{hh} directory,
which must be stored as \code{options()$HH.ROOT.DIR}.
If you installed the HH datasets as part of the R or S-Plus HH package, then
option \code{options()$HH.ROOT.DIR} is set automatically.

The code listings in Heiberger and Holland (2004) are
all given paths relative to the beginning of the \code{hh.file} directory,
which must be stored as \code{options()$HHfile.ROOT.DIR}.
Most of the files are designed to be entered at the command line, and
are not designed to be sourced.  The primary use of the \code{hh.file}
functions
is to display the pathname of the file so it can be opened for use in an editor.
The online files must be independently downloaded from the book's web site
\url{http://springeronline.com/0-387-40270-5}.
The \code{HHfile.ROOT.DIR} option must be set by the user to correspond
to the location where the files are stored.  The book recommends\cr
\code{options(HHfile.ROOT.DIR="c:/HOME/hh")} in Windows or\cr
\code{options(HHfile.ROOT.DIR="/usr/users/hh")} in Unix.
See Appendix B of Heiberger and Holland (2004) for further details.
The \code{options(HHfile.ROOT.DIR="something"} statement
may need to be modified to match the location of the online files directory on
your machine.  If you use more than one computer, you may need a
different value for the \code{HHfile.ROOT.DIR} option on each machine.  This
is the only change you will need to make in order to run any of our
software or examples.  The \code{hh}, \code{hh.file}, and
\code{hh.file.DOS} functions are not changed.
}
\value{
  Fully expanded, absolute pathname for the input filename.
  \code{hh} and \code{hh.file} use the separator convention of
  the \code{\link[base]{file.path}} function.  \code{hh.file.DOS}
  returns the pathname using the
  \samp{"\\\\"} separator convention.  When \code{displayForCutAndPaste}
  is \code{TRUE}, \code{hh.file.DOS} prints the full pathname with
the \samp{"\\"} convention, so it can be picked up and pasted into an editor that
uses the MS DOS convention for file paths.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## hotdog <- read.table(hh.old("datasets/hotdog.dat"), header=TRUE)
## This form of data input for files from the text has been replaced by
## the alternate form
##     data(hotdog)


\dontrun{
## Define the HHfile.ROOT.DIR option first.
## Define
##   options(HHfile.ROOT.DIR="c:/HOME/hh")  ## value recommended in Appendix B
## before using the hh.file() functions.
hh.file("relativefilepath")
hh.file.DOS("relativefilepath")
hh.file.DOS("relativefilepath", displayForCutAndPaste=FALSE)
}

}
\keyword{utilities }
\keyword{environment }
