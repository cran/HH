\name{anova.mean}
\alias{anova.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ANOVA table from the group sample sizes, means, and standard deviations.}
\description{
  Oneway ANOVA table from the summary information consisting of group
  sample sizes, means, and standard deviations.  The full dataset is not
  needed.
}
\usage{
anova.mean(object, n, ybar, s, ..., ylabel = "ylabel")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{level names}
  \item{n}{sample size for each level}
  \item{ybar}{sample mean for each level}
  \item{s}{sample standard deviation for each level}
  \item{\dots}{other arguments (not used)}
  \item{ylabel}{name of response variable}
}
\value{
  Analysis of variance table, identical to the ANOVA table
  that would have been produced by \code{anova.lm}
  if the original data, rather than the summary data, had been available.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
%% R
#ifndef S-Plus
  \code{\link[stats]{anova.lm}},
#endif
#ifdef S-Plus  
  \code{\link[splus]{anova.lm}},
#endif
  \code{\link{plot.mmc.multicomp}}
}
\examples{
## pulmonary data used in Hsu and Peruggia paper defining the mean-mean plot
## See ?plot.mmc.multicomp for details on the dataset.

pulmonary <- read.table(hh("datasets/pulmonary.dat"), header=TRUE,
                        row.names=NULL)
names(pulmonary)[3] <- "FVC"
names(pulmonary)[1] <- "smoker"
pulmonary$smoker <- factor(pulmonary$smoker, levels=pulmonary$smoker)
row.names(pulmonary) <- pulmonary$smoker
pulmonary

anova.mean(pulmonary$smoker,
           pulmonary$n,
           pulmonary$FVC,
           pulmonary$s,
           ylabel="pulmonary")
}
\keyword{models}
