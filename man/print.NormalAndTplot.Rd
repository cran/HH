\name{print.NormalAndTplot}
\alias{print.NormalAndTplot}
\title{Print method for Normal and t plots from NTplot.}
\description{Print method for Normal and t plots from NTplot.}
\usage{
\method{print}{NormalAndTplot}(x, tablesOnPlot=TRUE, plot=TRUE,
                scales=FALSE, prob=FALSE, call=FALSE,
                ..., cex.table=.7, digits=attr(x, "call.list")$digits,
                position.2=.17)
}
\arguments{
  \item{x}{A \code{"NormalAndTplot"} object.}
  \item{tablesOnPlot}{Logical.  If \code{TRUE}, display the tables in
    the \code{attr(x, "scales")} and \code{attr(x, "prob")} on the plot.}
  \item{plot}{Logical.  If \code{TRUE}, display the graph on the plot.}
  \item{scales, prob}{Logical. If \code{TRUE}, display the specified
    attribute on the R Console.}
  \item{call}{Logical.  If \code{TRUE}, display an R statement on the R console.}
  \item{\dots}{Other arguments are ignored.}
  \item{cex.table, digits}{\code{cex} and \code{digits} for the
    \code{tablesOnPlot}
    display of the \code{attr(x, "scales")} and \code{attr(x, "prob")} tables.}
  \item{position.2}{When \code{tablesOnPlot=TRUE}, the graph occupies
    the top of the device beginning at \code{position.2}.  This is the
    second value in the \code{position} argument of \code{\link[lattice]{print.trellis}}.}
}
\value{The argument is returned invisibly.}
\seealso{\code{\link{NTplot}}, \code{\link{NormalAndTplot}}.}
\author{Richard M. Heiberger (rmh@temple.edu)}
