\name{OneWayVarPlot}
\alias{OneWayVarPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displays a three-panel \code{bwplot} of the data by group, of the group
means, and of the entire dataset.  This is an approximate visualization
of the Mean Square lines from the ANOVA table for a one-way ANOVA model.
}
\description{
Displays a three-panel \code{bwplot} of the data by group, of the group
means, and of the entire dataset.  This is an approximate visualization
of the Mean Square lines from the ANOVA table for a one-way ANOVA model.
The groups are centered using medians by default.  Means, and anything
else, is an option.
}
\usage{
OneWayVarPlot(x, data, ...,
              main="Variability of Groups, Centers of Groups, and all Data",
              centerFunctionName="median",
              center=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Model formula with one response variable and one factor.
  }
  \item{data}{
    \code{data.frame}
  }
  \item{\dots}{
    Other arguments to be forwarded to the panel function.
  }
  \item{main}{
    \code{main} title for graph.
  }
  \item{centerFunctionName}{Name of centering function, with
    \code{"median"} as the default.  \code{"mean"} is another option.
  }
  \item{center}{
    Logical.  If \code{TRUE}, the default, the bwplots are centered by
    subtracting their center (by default the median).  If \code{FALSE}
    the bwplots are plotted at their observed values.
  }
}
\value{
  Three-panel \code{trellis} object.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(batch)
OneWayVarPlot(Calcium ~ Batch, data = batch)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
