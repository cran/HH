\name{hh}
\alias{hh}
\alias{HH.ROOT.DIR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Resolve filenames relative to the HH directory. }
\description{
  The pathnames for all the datasets and executable files in the online
  files accompanying Heiberger and Holland (2004) are given relative to
  the \code{options()$HH.ROOT.DIR} directory.
}
\usage{
hh(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Character string giving the pathname of a file in the HH
    online files relative to the \code{options()$HH.ROOT.DIR} directory.
  \code{file} is the name exactly as specified in Heiberger and Holland
  (2004).
The \code{hh} function resolves those names to the location where the
files are stored on your computer.}
}
\details{
The datasets and code listings in Heiberger and Holland (2004) are
all given paths relative to the beginning of the \code{hh} directory,
which must be stored as \code{options()$HH.ROOT.DIR}

If you installed the HH library from an R or S-Plus package, then
option \code{options()$HH.ROOT.DIR} is set automatically.

If you installed the HH library by unzipping the downloaded online
files (not a package),
then you must manually define the \code{HH.ROOT.DIR} option in
your working directory (for example, \code{"c:/HOME/yourname/.Data"} in
S-Plus, or the \code{.GlobalEnv} in R).  You may do this in your
\code{.First} function in either system.  Or you may enter at the
command prompt a statement of the form
\code{options(HH.ROOT.DIR="c:/HOME/hh")} in Windows or
\code{options(HH.ROOT.DIR="/usr/users/hh")} in Unix.
See Appendix B of Heiberger and Holland (2004) for further details.
The \code{options} statement
may need to be modified to match the location of the \code{hh} directory on
your machine.  If you use more than one computer, you may need a
different value for the \code{HH.ROOT.DIR} option on each machine.  This
is the only change you will need to make in order to run any of our
software or examples.  The \code{hh} function itself is unchanged.
}
\value{
Fully expanded, absolute pathname for the input filename.
}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
hotdog <- read.table(hh("datasets/hotdog.dat"), header=TRUE)
}
\keyword{utilities }
\keyword{environment }
