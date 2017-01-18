\name{NTplot}
\alias{NTplot}
\alias{NTplot.default}
\alias{NTplot.htest}
\alias{NTplot.power.htest}
\alias{NTplot.NormalAndTplot}
\title{Specify plots to illustrate Normal and t Hypothesis Tests or
  Confidence Intervals, including normal approximation to the binomial.}
\description{Specify plots to illustrate Normal and t Hypothesis Tests
  or Confidence Intervals, including normal approximation to the binomial.}
\usage{
NTplot(mean0, ...)
\method{NTplot}{default}(mean0=0, ..., shiny=FALSE,
        distribution.name = c("normal","z","t","binomial"))
\method{NTplot}{htest}(mean0, ..., shiny=FALSE, NTmethod="htest")
\method{NTplot}{power.htest}(mean0, ..., shiny=FALSE, xbar=NA, ## these input values are used
        mean1, n, df, sd, distribution.name, sub, ## these input values ignored
        alpha.left, alpha.right, number.vars) ## these input values ignored
        ## NTplot(NTplot(htest.object), n=20) ## allows override of arguments
\method{NTplot}{NormalAndTplot}(mean0, ..., shiny=FALSE)
}
\arguments{
  \item{mean0}{
    For the default method, \code{mean0} is either missing or a numeric
    argument for the mean under the null hypothesis.
    For the \code{htest} method, \code{mean0} is an \code{htest} object
    from the \code{\link[stats]{t.test}} or the
    \code{\link[TeachingDemos]{z.test}} function.
    For the \code{NormalAndTplot} method
    \code{mean0} is a \code{"NormalAndTplot"} object from a previous use
    of the \code{NTplot} function.
    For the \code{power.htest} method, \code{mean0} is a \code{power.htest}
    object from the \code{\link[stats]{power.t.test}} function.
  }
  \item{xbar}{See \code{\link{NormalAndTplot}}.}
  \item{\dots}{Other arguments, selected from the options for the
    default method \code{\link{NormalAndTplot}}.  Three named color schemes
    are available: the default \code{ntcolors="original"},
    \code{ntcolors="stoplight"}, and \code{ntcolors="BW"}.  Their
    definitions, along with information on specifying other color schemes, are
    shown in \code{\link{NormalAndTplot}}.
  }
  \item{shiny}{Logical.  If \code{TRUE}, a \code{\link[shiny]{shiny}}
    app is started to provide an interactive graphics device in a
    web-browser.  If
    \code{FALSE}, a plot is drawn on the current graphics device.
    For short browser windows (\code{height < 800} pixels), you may
    adjust the pixel height of the plot in the last user input field on
    the \code{Fonts} tab.
  }
  \item{htest}{logical. \code{TRUE} for \code{"htest"} objects.}
  \item{mean1, n, df, sd, sub, alpha.left,
    alpha.right, number.vars}{These variables are ignored here.  They
    are captured so they won't interfere with similarly named variables
    that are generated in the \code{power.htest} method.}
  \item{distribution.name}{Ignored by \code{htest} and
    \code{power.htest} methods.
    Otherwise passed on to the next method.}
  \item{NTmethod}{Character string used when \code{shiny=TRUE}.  It is
    normally calculated by the methods.  \code{NTmethod} tells
    \code{shiny} how to use or ignore the \code{df} and \code{n}
    sliders.  See the extended discussion in \code{\link{NormalAndTplot}}.
  }
  }
\details{
The graphs produced by this single function cover most of the first semester
introductory Statistics course.
All options of the \code{t.test}, \code{power.t.test}, and \code{z.test}
are accepted and displayed.

\code{NTplot} is built on \code{\link[lattice]{xyplot}}.
Most of the arguments detailed in \code{xyplot} documentation work to
control the appearance of the plot.

The shiny app (called when the argument \code{shiny=TRUE})
provides animated sliders for the means, standard
deviation, xlimits, significance levels, df, and n.  The df and n are
rounded to integers for the sliders (relevant for \code{htest} and
\code{power.htest} objects). Checkboxes and Radio buttons are available
for
various display options

When you have a graph on the shiny window that you wish to keep, click
on the "Display Options" tab, and then on the "Display Call" radio
button.  The main shiny window will show an R command which will
reproduce the current plot.  Pick it up with the mouse and drop it into
an R console window.

To get out of the shiny window and return to an interactive R console,
move the cursor back to the console window and interrupt the shiny call, usually
by entering \code{Ctrl-C} or \code{ESC}.

}
\value{
\code{"trellis"} object. The object can be plotted or fed back into the
\code{NTplot} function with argument \code{shiny=TRUE} to allow
interactive graphical investigation of the hypothesis test or confidence
interval.
The attributes of the object\\
\code{NTobj <- NTplot()}\\
\code{attr(NTobj, "scales")} and \code{attr(NTobj, "prob")} make the
data values and probability values accessible for further R
computations.
The \code{"call"} attribute
\code{cat(attr(NT.object, "call"), "\n")}
displays a statement that can be copied back into R to reproduce the graph.
The \code{cat()} is needed to unescape embedded quotes.
The \code{"call.list"} attribute
\code{attr(NT.object, "call.list")}
is a list that can be used with \code{do.call} to reproduce the graph.
\code{do.call(NTplot, attr(NT.object, "call.list"))}.  This is usually
not needed by the user because the simpler statement
\code{NTplot(NT.object)}
does it for you.
}
\seealso{\code{\link{NormalAndTplot}}, \code{\link{print.NormalAndTplot}}.}
\author{Richard M. Heiberger (rmh@temple.edu)}
\note{
  This function is built on \pkg{lattice} and \pkg{latticeExtra}.
  It supersedes the similar function
\code{normal.and.t.dist} built on base graphics that is used in many
  displays in the book by Erich Neuwirth and me: \emph{R through Excel}, Springer
(2009).
\url{http://www.springer.com/978-1-4419-0051-7}.  Many details,
  particularly the
  alternate color scheme and the concept of floating probability labels,
  grew out of discussions that Erich and I have had since the book was
  published.
  It incorporates ideas that Jay Kerns and I developed at the 2011 UseR! conference.
  This version incorporates some ideas suggested by Moritz Heene.
}
\keyword{ hplot }
\keyword{shiny}

\examples{
x1 <- rnorm(12)
x2 <- rnorm(12, mean=.5)

NT.object <- NTplot(mean0=0, mean1=1)
NT.object
attr(NT.object, "scales")
attr(NT.object, "prob")
cat(attr(NT.object, "call"), "\n") ## the cat() is needed to unescape embedded quotes.

NTplot(t.test(x1, x2))
NTplot(power.t.test(power = .90, delta = 1, alternative = "one.sided"))

\dontrun{
## 22 distinct calls are shown in
demo(NTplot, ask=FALSE)
}

\dontrun{ ## these are interactive and do not work in static checking of the code
NTplot(mean0=0, mean1=1, shiny=TRUE)
NTplot(shiny=TRUE, px.height=475) ## default value is 575
NTplot(t.test(x1, x2), shiny=TRUE, mean1=1)
NTplot(power.t.test(power = .90, delta = 1, alternative = "one.sided"), shiny=TRUE)
NTplot(NT.object, shiny=TRUE)

## run the shiny app
shiny::runApp(system.file("shiny/NTplot", package="HH"))
}

}
