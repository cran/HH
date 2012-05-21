\name{intxplot}
\alias{intxplot}
\alias{panel.intxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interaction plot, with an option to print standard error bars.
}
\description{
  Interaction plot, with an option to print standard error bars.
  There is an option to offset group lines to prevent the
  bars from overprinting.
}
\usage{
intxplot(x, data=sys.parent[1], groups.in,
          scales,
          key.length=1,
          key.lines,
          key=TRUE,
          trace.factor.name=deparse(substitute(groups.in)),
          x.factor.name=x.factor,
          xlab=x.factor.name,
          main=list(main.title, cex=main.cex),
          condition.name="condition",
          panel="panel.intxplot",
          summary.function="sufficient",
          se,
          ...,
          data.is.summary=FALSE,
          main.title=paste(
            "Interactions of", trace.factor.name, "and",
            x.factor.name,
            if (length(x[[3]]) > 1)
            paste("|", condition.name.to.use)),
          main.cex=1.5)

panel.intxplot(x, y, subscripts, groups, type = "l", ..., se, cv=1.96,
               offset.use=(!missing(groups) && !missing(se)),
               offset.scale=2*max(as.numeric.positioned(groups)),
               offset=
               as.numeric.positioned(groups[match(levels(groups),
                                            groups)]) / offset.scale,
               rug.use=offset.use)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{For \code{intxplot}, a formula with a factor as the
    predictor variable.
   For \code{panel.intxplot}, standard argument for panel functions.}
  \item{data}{data.frame, as used in \code{xyplot}.}
  \item{groups.in}{\code{groups.in}, as used in \code{xyplot}.}
  \item{scales}{Optional, additional arguments for the standard \code{scales}
    in \code{xyplot}.}
  \item{key.length}{Number of columns in the key.}
  \item{key.lines}{default value for the \code{lines} argument of \code{key}.}
  \item{key}{logical.  If \code{TRUE}, draw the key.}
  \item{trace.factor.name}{Name of the grouping variable.}
  \item{x.factor.name}{name of the dependent variable.}
  \item{xlab}{as in \code{xyplot}, defaults to the name of the predictor variable
    from the formula.}
  \item{main}{as in \code{xyplot}.  Defaults to the \code{main.title} argument.}
  \item{panel}{as in \code{xyplot}.  Defaults to the \code{"panel.intxplot"}.}
  \item{condition.name}{name of the conditioning variable.}
  \item{summary.function}{The default \code{sufficient} finds the mean,
    standard deviation, and sample size of the response variable for each
    level of the conditioning factor. See \code{\link{sufficient}}.}
  \item{se}{standard errors to be passed to \code{panel.intxplot}.
    \code{se} Missing, logical, or a numeric vector.
    If missing or \code{FALSE}, standard errors are not plotted.
    If \code{se=TRUE} in \code{intxplot},
    the standard errors are calculated from the
    sufficient statistics for each group as the group's standard deviation
    divided by the square root of the group's observation
    count.  If \code{se} is numeric vector, it is evaluated in the environment of
    the sufficient statistics.
    the \code{se} argument to \code{panel.intxplot} must be numeric.
  },
  \item{\dots}{In \code{intxplot}, arguments for \code{panel.intxplot}.
    In \code{panel.intxplot}, arguments for \code{panel.superpose}.}
  \item{data.is.summary}{logical, defaults to \code{FALSE} under the
    assumption that the input data.frame is the original data and the
    \code{intxplot} function will generate the summary information
    (primarily standard deviation \code{sd} and number of
    observations \code{nobs} for each group).  When \code{TRUE}, the
    standard error calculation assumes variables \code{sd} and
    \code{nobs} are in the dataset.}

  \item{main.title}{Default main title for plot.}
  \item{main.cex}{Default character expansion for main title.}

  %% panel.intxplot

  \item{y, subscripts, groups, type}{Standard arguments for panel functions.}
  \item{cv}{critical value for confidence intervals. Defaults to 1.96.}
  \item{offset.use}{logical.  If \code{TRUE}, offset the endpoints
  of each group.}
  \item{offset.scale}{Scale number indicating how far apart the
    ends of the groups will be placed.  Larger numbers make them closer
    together.}
  \item{offset}{Actual numbers by which the end of the groups are offset
  from their nominal location which is the \code{as.numeric} of the
  group levels.}
  \item{rug.use}{logical. If \code{TRUE}, display a rug for the endpoints
  of each group.}
}
\value{
\code{"trellis"} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{sufficient}}}
\examples{
## This uses the same data as the HH Section 12.13 rhizobium example.

data(rhiz.clover)

## interaction plot, no se
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover)

## interaction plot, individual se for each treatment combination
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover, se=TRUE)

## Rescaled to allow the CI bars to stay within the plot region
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover, se=TRUE,
         ylim=range(rhiz.clover$Npg))

## interaction plot, common se based on ANOVA table
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover,
         se=sqrt(sum((nobs-1)*sd^2)/(sum(nobs-1)))/sqrt(5))

## Rescaled to allow the CI bars to stay within the plot region
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover,
         se=sqrt(sum((nobs-1)*sd^2)/(sum(nobs-1)))/sqrt(5),
         ylim=range(rhiz.clover$Npg))

## change distance between endpoints
intxplot(Npg ~ strain, groups=comb, data=rhiz.clover,
         se=TRUE, offset.scale=20)

## When data includes the nobs and sd variables, data.is.summary=TRUE is needed.
intxplot(Npg ~ strain, groups=comb,
         se=sqrt(sum((nobs-1)*sd^2)/(sum(nobs-1)))/sqrt(5),
         data=sufficient(rhiz.clover, y="Npg", c("strain","comb")),
         data.is.summary=TRUE,
         ylim=range(rhiz.clover$Npg))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
