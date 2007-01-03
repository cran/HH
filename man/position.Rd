\name{position}
\alias{position}
\alias{position<-}
\alias{is.numeric.positioned}
\alias{as.numeric.positioned}
\alias{as.double.positioned}
\alias{[.positioned}
\alias{as.positioned}
\alias{is.positioned}
\alias{positioned}
\alias{print.positioned}
\alias{unique.positioned}
\alias{unpositioned}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find or assign the implied position for graphing the levels of a
  factor.
  A new class "positioned", which inherits from "ordered" and "factor",
  is defined.}
\description{
  The default values for plotting a factor \code{x} are the integers
  \code{1:length(levels(x))}.  These functions provide a way of
  specifying alternate plotting locations for the levels.
}
\usage{
position(x)

position(x) <- value

is.numeric.positioned(x, ...)
as.numeric.positioned(x, ...)
as.double.positioned(x, ...)
x[..., drop=FALSE]
"[.positioned"(x, ..., drop=FALSE)
as.positioned(x)
is.positioned(x)
positioned(x, ..., value)
print.positioned(x, ...)
unique.positioned(x, incomparables = FALSE, ...)
unpositioned(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector or factor}
  \item{value}{numerical values to be
    associated with \code{levels(x)}.
  The \code{length(value)} must equal \code{length(levels(as.factor(x)))}.}
\item{\dots}{other arguments.}
\item{drop}{See \code{\link[base]{"["}}in R or \code{\link[main]{"["}}
  in S-Plus.}
\item{incomparables}{See \code{\link[base]{unique}}in R or
  \code{\link[splus]{unique}} in S-Plus.}
}
\value{

  \code{position(x) <- value} first forces its argument to be an ordered
  factor and then assigns the \code{value} to the \code{"position"}
  attribute of the ordered factor.
  The result is assigned class \code{"positioned"} and returned.

  \code{position(x)} returns the position values associated with
  \code{levels(x)}.  If \code{x} is a positioned factor, then
  the \code{"postion"} attribute is returned.
  If \code{x} is a factor, or an ordered factor whose
  levels cannot be coerced to numeric, then the integers
  \code{1:length(levels(x))} are returned.  If \code{x} is an an ordered
  factor whose levels can be coerced to numeric, then the numeric values
  corresponding to the levels are returned.  If \code{x} is numeric,
  then \code{x} itself is returned.

  \code{as.numeric.positioned(x)} returns a numeric vector the length of the
  original vector.  If \code{x} inherits from \code{"factor"},
  then the values in the vector are the values in
  \code{position(x)} subscripted by the levels of the factor.
  If \code{x} is numeric, then \code{x} itself is returned.

  \code{unpositioned(x)} removes the \code{"position"} attribute and
  removes the \code{"positioned"} value from the the \code{oldClass} of
  the object.

}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{panel.interaction2wt}}, \code{\link[base]{factor}}
  in R, \code{\link[splus]{factor}} in S-Plus.}
\examples{
## ordered with character levels defaults to
## integer position of specified levels
tmp <- ordered(c("mm","cm","m","m","mm","cm"),
               levels=c("mm","cm","m")) ## size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)

## position is assigned to ordered in specified order
tmp <- ordered(c("cm","mm","m","m","mm","cm"),
               levels=c("mm","cm","m")) ## size order
levels(tmp)
position(tmp) <- c(-3, -2, 0) ## log10 assigned in size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)
unpositioned(tmp)
unique(tmp)

## numeric stays numeric
tmp <- c(0.010, 0.001, 1.000, 1.000, 0.001, 0.010)
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)

## factor with numeric levels, position is integer position in size order
tmp <- factor(c(0.010, 0.001, 1.000, 1.000, 0.001, 0.010))
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)

## ordered with numeric levels, position is numeric value in size order
tmp <- ordered(c(0.010, 0.001, 1.000, 1.000, 0.001, 0.010))
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)

## factor with numeric levels
## position is assigned in size order
tmp <- factor(c(0.010, 0.001, 1.000, 1.000, 0.001, 0.010))
levels(tmp)
position(tmp) <- c(-3, -2, 0) ## log10 assigned in size order
tmp
as.numeric(tmp)
levels(tmp)
position(tmp)
as.numeric(tmp)



## boxplots coded by week
tmp <- data.frame(Y=rnorm(40, rep(c(20,25,15,22), 10), 5),
                  week=ordered(rep(1:4, 10)))
position(tmp$week) <- c(1, 2, 4, 8)

if.R(r=
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
tmp <- factor(c("cm","mm","m","m","mm","cm")) ## default alphabetic order
tmp
as.numeric(tmp)
levels(tmp)   ## you probably DON'T want to do this!
position(tmp) ## you probably DON'T want to do this!
as.numeric(tmp)
##
## position is assigned to factor in default alphabetic order.
## you probably DON'T want to do this!
tmp <- factor(c("cm","mm","m","m","mm","cm"))
levels(tmp)
position(tmp) <- c(-3, -2, 0) ## assigned in default alphabetic order
tmp
as.numeric(tmp)
levels(tmp)   ## you probably DON'T want to do this!
position(tmp) ## you probably DON'T want to do this!
as.numeric(tmp)

}
\keyword{dplot}
