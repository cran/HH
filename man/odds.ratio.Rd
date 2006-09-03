\name{odds.ratio}
\alias{odds.ratio}
\alias{plot.odds.ratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate or plot the odds ratio for a 2x2 table of counts.}
\description{
  Calculate or plot the odds ratio for a 2x2 table of counts.
  The plot shows the confidence intervals on the probability of
  row2 for fixed odds ratio
  and specified probability for row1.
}
\usage{
odds.ratio(x, alpha = 0.05)

plot.odds.ratio(x,
 ylab = "prob(col1 | row1)", xlab = "prob(col1 | row2)",
 alpha = c(0.05, 0.5),
 legend.x=1.05,
 oma=c(0,0,0,5), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{2 x 2 table of counts}
  \item{alpha}{Significance levels of test.
    \code{odds.ratio} requires a single number in the range [0,1].
    \code{plot.odds.ratio} accepts more than one number on the range
    [0,1] and draws confidence lines at each value.}  \item{xlab,
    ylab}{x- and y-labels for the plot.  Sensible defaults are generated.}
  \item{\dots}{other arguments, currently ignored. }
  \item{legend.x}{x position of left-hand side of legend.}
  \item{oma}{outer margin \code{par()$oma}, needed to make room for legend. }
}
\value{
  \code{plot.odds.ratio} draws a plot and invisibly returns the same
  list as \code{odds.ratio} for the first value of \code{alpha}.
  \code{odds.ratio} returns the list:
\item{p1, p2}{proportion of each row total observed in the first column.}          
\item{omega1, omega2}{odds for each row, p/(1-p)}        
\item{psihat}{odds ratio, omega2/omega1}      
\item{s.ln.psihat}{standard deviation of \code{ln(psihat)}} 
\item{ci.ln.psihat}{confidence interval for \code{ln(psihat)}
  using normal approximation}
\item{ci.psihat}{confidence interval for \code{psihat} calculated
  ase \code{p(ci.ln.psihat)}}   
\item{prob1}{\code{seq(0,1,.05)}, set of p1 values for plotting.}         
\item{odds1}{\code{p1/(1-p1)}}
\item{odds2}{odds for the second row needed to retain \code{psihat} with
  the specified \code{odds1}, calculated as \code{odds1*psihat}.}       
\item{ci.odds2}{confidence interval for \code{odds2}}    
\item{prob2}{\code{odds2 / (odds2+1)}}       
\item{ci.prob2}{\code{ci.odds2 / (ci.odds2+1)}}    

}
\references{ Heiberger, Richard~M. and Holland, Burt (2004b).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in S-Plus, R, and SAS}.
 Springer Texts in Statistics. Springer.
 ISBN 0-387-40270-5.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\examples{
glasses <- read.table(hh("datasets/glasses.dat"), header=FALSE)

glasses <- matrix(unlist(glasses),
                  nrow=2,
                  dimnames=list(
                    c("glasses","no.glasses"),
                    c("delinq","non.del")))


## draw the iso-odds ratio plot with 50\% CI and 95\% CI,
## invisibly return the 95\% CI
plot.odds.ratio(glasses)
}
\keyword{hplot }
\keyword{htest }
