\name{interaction2wt}
\alias{interaction2wt}
\alias{interaction2wt.formula}
\alias{interaction2wt.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot all main effects and twoway interactions in a multifactor design}
\description{
The main diagonal
displays boxplots for the main effects of each factor.  The
off-diagonals show the interaction plots for each pair of factors.
The \code{i,j} panel shows the same factors as the \code{j,i} but with
the trace- and x-factor roles interchanged.
}
\usage{
interaction2wt(x, ...)

\method{interaction2wt}{formula}(x, data=NULL, responselab, ...)

\method{interaction2wt}{default}(x,
               response.var,
               responselab = deparse(substitute(response.var)),
               responselab.expression = responselab,
               relation = list(x = "same", y = "same"),
               x.relation = relation$x,
               y.relation = relation$y,
               digits = 3,
               x.between=1,
               y.between=1,
               between,
               cex = 0.75,
               rot=c(0,0),
               panel.input = panel.interaction2wt,
               strip.input =
                if (label.as.interaction.formula) strip.default
                else strip.interaction2wt,
               par.strip.text.input = trellis.par.get()$add.text,
               scales.additional,
               main.in =
                 paste(responselab,
                       ": ", c("main", "simple")[1+simple],
                       " effects and 2-way interactions",
                       sep=""),
               xlab = "",
               ylab = "",
               simple=FALSE,
               box.ratio=if (simple) .32 else 1,
               label.as.interaction.formula=TRUE,
               ...,
               main.cex,
               key.cex.title=.8,
               key.cex.text=.7,
               factor.expressions=names.x,
               simple.pch=NULL,
               col.by.row=TRUE,
               col  =trellis.par.get("superpose.line")$col,
               lty  =trellis.par.get("superpose.line")$lty,
               lwd  =trellis.par.get("superpose.line")$lwd,
               alpha=trellis.par.get("superpose.line")$alpha
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{Arguments when \code{x} is a formula.
  \item{x}{
    The object on which method dispatch is carried out.

    For the \code{"formula"} method, a formula describing the response
    variable and factors.
    The formula is generally of the form \code{y ~ g1 + g2 + \dots}.
    There may be one or more factors in the formula.

    For the \code{"default"} method, \code{data.frame} of factors.  This
    is usually constructed by \code{formula} method from the input
   data and the input formula.
}

  \item{data}{
    For the \code{formula} method, a data frame containing values for
    any variables in the formula.  In the R version,
    if not found in \code{data}, or if
    \code{data} is unspecified, the variables are looked for in the
    environment of the formula.
}
  \item{responselab}{Character name of response variable, defaults to
    the name of the response variable in the \code{formula}.}
  \item{responselab.expression}{\code{plotmath} or character name
    of response variable, defaults to \code{responselab}.
  }
  \item{\dots}{additional arguments, primarily trellis arguments.}

  \item{response.var}{For the \code{"default"} method, the
    response variable.  This is usually constructed by
    \code{formula} method from the input data and the input formula.}

  \item{simple}{logical.  \code{TRUE} if simple effects are to be
    displayed.  Arguments \code{simple.offset}, \code{simple.scale}, and
    \code{col.by.row} may also be needed.  See
    \code{\link{panel.interaction2wt}} for details.}
  \item{box.ratio}{
%% R
#ifndef S-Plus
    \code{\link[lattice]{xyplot}}.
#endif
#ifdef S-Plus
    \code{\link[trellis]{bwplot}}.
#endif
  }

Trellis/Lattice arguments.  Default values are set by the
the \code{formula} method.  The user may override the defaults.
See also
%% R
#ifndef S-Plus
\code{\link[lattice]{xyplot}}.
#endif
#ifdef S-Plus
\code{\link[trellis]{trellis.args}}.
#endif

  \item{relation}{\code{trellis} argument.}
  \item{x.relation}{\code{x} value of \code{relation} argument.}
  \item{y.relation}{\code{y} value of \code{relation} argument.}
  \item{digits}{doesn't do anything at the moment}
  \item{x.between}{\code{x} value of \code{between} argument.}
  \item{y.between}{\code{y} value of \code{between} argument.}
  \item{between}{trellis/lattice \code{between} argument.
    If used, \code{between} has precedence over both the
    \code{x.between} and \code{y.between} arguments.}
  \item{cex}{S-Plus: changes the size of the median dot in the
  boxplots.  R: doesn't do anything.}
  \item{panel.input}{panel function.  Default is \code{panel.interaction2wt}.}
  \item{label.as.interaction.formula}{logical.  If \code{TRUE}, each
  panel has a single strip label of the form \code{y ~ a | b}.  If
  \code{FALSE}, each panel has a pair of strip labels, one for the
  \code{trace} factor and one for the \code{x} factor.}
  \item{strip.input}{strip function.  Default depends on the value of
  \code{label.as.interaction.formula}.}
  \item{par.strip.text.input}{\code{par.strip.text} argument.}
  \item{scales.additional}{additional arguments to \code{scales}
  argument of \code{\link{interaction.positioned}}.}
  \item{main.in}{Text of main title.}
%  \item{xlab.in}{S-Plus: FALSE suppresses x labels in trellis.  R: no effect.}
  \item{xlab}{No effect.}
  \item{ylab}{No effect.}
  \item{main.cex}{\code{cex} for main title.}
  \item{key.cex.title}{\code{cex} key title.  Defaults to \code{cex} for
  \code{xlab}.}
  \item{key.cex.text}{\code{cex} group names in key.  Defaults to \code{cex} for
  \code{axis.text}.}
  \item{factor.expressions}{Expressions for titles of keys and
  \code{xlab} for each column.  Defaults to the names of the factors in
  the input formula.}
  \item{rot}{Rotation of x tick labels and y tick labels.  Only 0 and 90
  will look good.}
  \item{simple.pch}{Named list containing plotting characters for each
    level of one or more of the factors.  \code{simple.pch} is used only when
    \code{simple==TRUE}.  If the argument \code{simple.pch} is missing,
    then the integers for the levels of the factors are used.  The
    characters are used for the median of the box plots in the diagonal
    panels.  They match the trace factor of the interaction panel in the
    same column of the display.  }
  \item{col.by.row}{logical.  If \code{TRUE} (the default), simple
    effects plots color the simple effects on the main diagonals in the same
    color as the trace levels in their row.  If \code{FALSE}, then simple
    effects are colored to match the x levels in their column.}
  \item{col, lty, lwd, alpha}{Arguments to
    \code{trellis.par.set(superpose.line=list())}.}
}
\value{
\code{"trellis"} object containing the plot.
}
\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course with Examples in R}.
Second Edition.
Springer-Verlag, New York.
\url{https://link.springer.com/book/10.1007/978-1-4939-2122-5}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{panel.interaction2wt}}}
\examples{
data(vulcan)

interaction2wt(wear ~ filler + pretreat + raw, data=vulcan,
               par.strip.text=list(cex=.7))

interaction2wt(wear ~ filler + pretreat + raw, data=vulcan,
               par.strip.text=list(cex=.7),
               label.as.interaction.formula=FALSE)

interaction2wt(wear ~ filler + raw, data=vulcan,
               simple=TRUE)

interaction2wt(wear ~ filler + raw, data=vulcan,
               simple=TRUE, col.by.row=FALSE)

interaction2wt(wear ~ filler + raw, data=vulcan,
               simple=TRUE, simple.scale=c(filler=.15, raw=.2),
               xlim=c(.3, 5.6))

interaction2wt(wear ~ filler + raw, data=vulcan,
               col=1:5, lwd=1:5, lty=1:5)

interaction2wt(wear ~ filler + raw, data=vulcan,
               simple=TRUE, col=1:5, lwd=1:5, lty=1:5)

interaction2wt(wear ~ filler + raw, data=vulcan,
               simple=TRUE, col=1:5, lwd=1:5, lty=1:5, col.by.row=FALSE,
               simple.pch=list(filler=LETTERS[1:5], raw=letters[6:9]), cex=2)



ToothGrowth$dose <- positioned(ToothGrowth$dose) ## modify local copy
anova(aov(len ~ supp*dose, data=ToothGrowth))
interaction2wt(len ~ supp + dose, data=ToothGrowth)


esoph$ntotal <- with(esoph, ncases + ncontrols) ## modify local copy
esoph$rate   <- with(esoph, ncases/ntotal)      ## modify local copy

position(esoph$alcgp) <- 2:5
position(esoph$tobgp) <- 2:5

interaction2wt(rate ~ agegp + alcgp + tobgp, esoph, rot=c(90,0),
               par.strip.text=list(cex=.8))

interaction2wt(rate ~ agegp + alcgp + tobgp, esoph, rot=c(90,0),
               par.strip.text=list(cex=.8),
               factor.expressions=c(
                 agegp=expression(Age~~(years)),
                 alcgp=expression(Alcohol~
                   bgroup("(",scriptstyle(frac(gm, day)),")")),
                 tobgp=expression(Tobacco~
                   bgroup("(",scriptstyle(frac(gm, day)),")"))),
               par.settings=list(
                 par.xlab.text=list(cex=.8),
                 par.ylab.text=list(cex=.8)),
               responselab.expression="Cancer\nRate",
               main=list(
"Esophogeal Cancer Rate ~ Alcohol Consumption + Tobacco Consumption",
                 cex=1.2))

esoph.aov <- aov(rate ~ agegp + alcgp + tobgp, data=esoph)
anova(esoph.aov)

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ design }% __ONLY ONE__ keyword per line
