\name{latexSetOptions}
\alias{latexSetOptions}
\alias{latexCheckOptions}
\title{Set the options for use of latex; check whether the options
  for latex functions have been specified.}
\description{
  Set the options for use of latex; check
  whether the options for latex functions have been specified:
if any of\cr
\code{options()[c("latexcmd","dviExtension","xdvicmd")]}
are \code{NULL}, an error message is displayed.
}
\usage{
latexSetOptions(
     latexcmd=c("pdflatex", "latex"),
     dviExtension={
       if (is.null(latexcmd)) NULL
       else
         switch(latexcmd,
                pdflatex="pdf",
                latex="dvi")
     },
     xdvicmd={
       if (is.null(latexcmd)) NULL ## dvips is used, .ps in wd displayed
       else
         switch(latexcmd,
                pdflatex=if (nchar(Sys.which("open")))
                           "open"      ## Macintosh, Windows, SMP linux
                         else
                           "xdg-open", ## ubuntu linux
                latex="dvips") ##
                               ## dvips  Mac, Win: .ps in wd displayed
                               ## xdvi   Mac: Quartz displays image borders
                               ##             and waits until dismissed.
                               ## xdvi   Windows: not on my machine.
                               ## yap    Windows: dvi is displayed
                               ## open   Mac: nothing happens
                               ## open   Windows: yap displays dvi
     }
     )

latexCheckOptions(...)
}
\arguments{
  \item{latexcmd, dviExtension, xdvicmd}{See \code{\link[Hmisc]{latex}}.}
  \item{\dots}{
Any arguments to \code{latexCheckOptions} are ignored.
}
}
\value{
  For \code{latexSetOptions}, the invisible list of the options that were set
  by this command.

  For \code{latexCheckOptions}, if any \code{NULL} options are detected, the
  error message is printed.  If all three options have non-\code{NULL} values, NULL.
}
\details{
  These are my recommendations (the default when no arguments are
  specified)
  for \code{pdflatex}:\cr
  \preformatted{
    options(latexcmd="pdflatex") ## Macintosh, Windows, linux
    options(dviExtension="pdf")  ## Macintosh, Windows, linux

    if (nchar(Sys.which("open"))) {
      options(xdvicmd="open")      ## Macintosh, Windows, SMP linux
    } else {
      options(xdvicmd="xdg-open")  ## ubuntu linux
    }
  }

  These are my recommendations for \code{latex} (and are the settings
  when only the first argument is set to \code{"latex"}):\cr
  \preformatted{
    options(latexcmd="latex")
    options(dviExtension="dvi")
    options(xdvicmd="dvips")
  }
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[Hmisc]{latex}}
}
\examples{
\dontrun{
  latexSetOptions() ## default
  latexSetOptions("pdflatex") ## same as default
  latexSetOptions("pdflatex", "pdf", "open") ## same as default on Macintosh, Windows, SMP Unix

  latexSetOptions("latex")
  latexSetOptions("latex", "dvi", "dvips") ## same as above

  latexSetOptions(NULL)
  latexSetOptions(NULL, NULL, NULL) ## same as above
}
}
\keyword{ utilities}
