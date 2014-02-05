\name{panel.bwplot.superpose}
\alias{panel.bwplot.superpose}
\alias{panel.bwplot.groups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Panel function for bwplot that displays an entire box in the colors coded
by groups.
}
\description{
Panel function for bwplot that displays an entire box (central dot, box,
umbrella, outliers) in the same color, coded by the groups argument.
The function is based on \code{panel.superpose}.
}
\usage{
panel.bwplot.superpose(x, y, ...,
                       groups=groups,
                       col=rep(trellis.par.get("superpose.symbol")$col,
                               length=length(groups)),
                       pch=trellis.par.get("box.dot")$pch,
                       panel.groups=panel.bwplot.groups)

panel.bwplot.groups(..., col, pch, fill, fill.alpha=NULL, group.number)
}
\arguments{
  \item{x, y}{
Standard arguments to a \pkg{lattice} panel function.
When \code{x} has class \code{positioned} (see \code{\link{position}}), the
  position  will be forwarded by \code{panel.bwplot.superpose} to \code{panel.bwplot.groups}.
}
  \item{\dots}{
Additional \pkg{lattice} arguments.
}
  \item{groups}{
Factor to be used for color coding entire boxes: central dot,
  rectangle, umbrella, and outlier symbol.
}
  \item{col}{
Colors to be assigned to the levels of the group.  The default colors
  are taken from \code{trellis.par.get("superpose.symbol")$col}.
}
  \item{pch}{
Standard \pkg{lattice} arguments.
The \code{pch} describes the central dot.  The outlier dots are
specified in the \code{plot.symbol} component of \code{trellis.par.get}.
}
  \item{fill, fill.alpha}{
These are related to the similarly named arguments in \code{panel.bwplot}.
The \code{fill} argument is ignored.  It is there to capture the
automatically
generated \code{fill} argument.  The default\code{NULL} value of \code{fill.alpha}
implies that there is no background color for the boxes.  The user can set
\code{fill.alpha}
to a number between 0 and 1.  The boxes will be shaded in a lighter
version of their color as implied by the \code{groups} argument.  The
value 0 gives a transparent fill, and the value one makes the box the
full opaque color.
}
  \item{panel.groups, group.number}{
See \code{\link[lattice]{panel.superpose}} for details.
}
}
\details{
\code{panel.bwplot.superpose} is the user-level function.
\code{panel.bwplot.groups} is the \code{panel.groups} function called by \code{panel.superpose}.

}
%% \value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%% }
\author{ Richard M. Heiberger <rmh@temple.edu> }

\seealso{
\code{\link{position}}, \code{\link{panel.bwplot.intermediate.hh}},
\code{\link[lattice]{panel.superpose}}
}
\examples{
tmp <- data.frame(Response=rnorm(20), Group=factor(rep(LETTERS[1:3], c(5,7,8))))

bwplot(Group ~ Response, data=tmp,
       main="Default panel.bwplot, groups ignored", groups=Group)

bwplot(Group ~ Response, data=tmp,
       main="panel.bwplot.superpose",
       groups=Group, panel=panel.bwplot.superpose)

bwplot(Group ~ Response, data=tmp,
       main="panel.bwplot.superpose with fill specified",
       groups=Group, panel=panel.bwplot.superpose,
       fill.alpha=.33)


bwplot(Group ~ Response, data=tmp,
       main="panel.bwplot.superpose, with color specified",
       groups=Group, panel=panel.bwplot.superpose,
       col=c("forestgreen","blue", "brown"))


Test <- data.frame(id=rep(letters, each=4),
                   Week=rep(c(0,1,3,6), 26),
                   Treatment=rep(c("A","B"), each=52),
                   y=rep(1:4, 52) + rep(4:5, each=52) + rnorm(104))
Test$WeekTrt <- with(Test, interaction(Week, Treatment))
position(Test$Week) <- unique(Test$Week)
position(Test$WeekTrt) <- as.vector(outer(position(Test$Week), c(-.2, .2), `+`))

tapply(Test$y, Test[c("Week", "Treatment")], median)

bwplot( y ~ WeekTrt, groups = Treatment, data = Test,
      main="default panel.bwplot, groups ignored")

bwplot( y ~ WeekTrt, groups = Treatment, data = Test,
      panel=panel.bwplot.superpose,
      scales=list(x=list(limits=c(-1, 7))),
      main="Minimal use of panel.bwplot.superpose")

bwplot( y ~ WeekTrt, groups = Treatment, data = Test,
       panel=panel.bwplot.superpose,
       scales=list(x=list(limits=c(-1, 7), at=position(Test$Week))),
       box.width=.3,
       xlab="Week",
       pch=c(17, 16),
       key=list(col=trellis.par.get()$superpose.symbol$col[1:2],
           border=TRUE, title="Treatment", cex.title=1, columns=2,
           text=list(levels(Test$Treatment)),
           points=list(pch=c(17, 16))),
       par.settings=list(plot.symbol=list(pch=c(17, 16), cex=.5)),
       main="panel.bwplot.superpose with additional annotations")


bwplot( y ~ WeekTrt, groups = Treatment, data = Test,
       panel=panel.bwplot.superpose,
       scales=list(x=list(limits=c(-1, 7), at=position(Test$Week))),
       box.width=.3,
       xlab="Week",
       pch=c(17, 16),
       key=list(col=trellis.par.get()$superpose.symbol$col[1:2],
           border=TRUE, title="Treatment", cex.title=1, columns=2,
           text=list(levels(Test$Treatment)),
           points=list(pch=c(17, 16))),
       par.settings=list(plot.symbol=list(pch=c(17, 16), cex=.5)),
       main="panel.bwplot.superpose with fill and more complex panel.groups",
       panel.groups = function(...) {
           panel.stripplot(...)
           panel.bwplot.groups(...)
       },
       fill.alpha=.33,
       jitter.data = TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot }
