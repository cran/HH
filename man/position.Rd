\name{position}
\alias{position}
\alias{position<-}
\alias{as.position}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find or assign the implied position for graphing the levels of a factor.}
\description{
  The default values for plotting a factor \code{x} are the integers
  \code{1:length(levels(x))}.  These functions provide a way of
  specifying alternate plotting locations for the levels,
}
\usage{
position(x)

position(x) <- value

as.position(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector or factor}
  \item{value}{numerical values to be associated with \code{levels(x)}.
  The \code{length(value)} must equal \code{length(levels(as.factor(x)))}.}
}
\value{
  \code{position(x) <- value} forces its argument to be a factor and
  then assigns a \code{"position"} attribute to the factor.
  The new factor with the \code{"position"} attribute is returned.

  \code{position(x)} returns the numeric values associated with
  \code{levels(x)}.  If \code{x} is numeric, then \code{x} is returned.

  \code{as.position(x)} returns a numeric vector the length of the
  original vector.  If \code{x} is numeric, then \code{x} is returned.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
## ordered with character levels defaults to
## integer position of specified levels
tmp <- ordered(c("mm","cm","m","m","mm","cm"), c("mm","cm","m")) ## size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

## position attribute is assigned to ordered in specified order
tmp <- ordered(c("mm","cm","m","m","mm","cm"), c("mm","cm","m")) ## size order
attr(tmp,"position") <- c(-3, -2, 0) ## log10 assigned in size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

## numeric stays numeric
tmp <- c(0.001, 0.010, 1.000, 1.000, 0.001, 0.010)
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

## factor with numeric levels, defaults to integer position
tmp <- factor(c(0.001, 0.010, 1.000, 1.000, 0.001, 0.010))
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

## ordered with numeric levels, defaults to numeric value
tmp <- ordered(c(0.001, 0.010, 1.000, 1.000, 0.001, 0.010)) ## size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

## factor with numeric levels
## position attribute is assigned in size order
tmp <- factor(c(0.001, 0.010, 1.000, 1.000, 0.001, 0.010))
attr(tmp,"position") <- c(-3, -2, 0) ## log10 assigned in size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)


## boxplots coded by week
tmp <- data.frame(Y=rnorm(40, rep(c(20,25,15,22), 10), 5),
                  week=ordered(rep(1:4, 10)))
position(tmp$week) <- c(1, 2, 4, 8)

if.R(r=  ## NOT RIGHT YET
       bwplot(Y ~ week, horizontal=FALSE,
              scales=list(x=list(limits=c(0,9),
                          at=position(tmp$week),
                          labels=position(tmp$week))),
              data=tmp, panel=panel.bwplot.intermediate.hh)
    ,s=
       t(bwplot(week ~ Y, at=position(tmp$week),
                scales=list(y=list(limits=c(0,9),
                            at=position(tmp$week), labels=position(tmp$week))),
                data=tmp, panel=panel.bwplot.intermediate.hh))
    )


#### You probably don't want to use the next two examples.
#### You need to be aware of their behavior.
##
## factor with character levels defaults to
## integer position of sorted levels.
## you probably DON'T want to do this!
tmp <- factor(c("mm","cm","m","m","mm","cm")) ## default alphabetic order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)
##
## position attribute is assigned to factor in default alphabetic order.
## you probably DON'T want to do this!
tmp <- factor(c("mm","cm","m","m","mm","cm"))
attr(tmp,"position") <- c(-3, -2, 0) ## assigned in default alphabetic order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.position(tmp)

}
\keyword{dplot}
