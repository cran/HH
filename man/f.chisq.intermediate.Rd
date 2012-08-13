\name{dchisq.intermediate}
\alias{dchisq.intermediate}
\alias{pchisq.intermediate}
\alias{qchisq.intermediate}
\alias{df.intermediate}
\alias{pf.intermediate}
\alias{qf.intermediate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Intermediate f and chisq functions to simplify writing
  for both R and S-Plus.}
\description{
Intermediate f and chisq functions to simplify writing for both R and S-Plus.
}
\usage{
dchisq.intermediate(x, df, ncp=0, log=FALSE)
pchisq.intermediate(q, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
qchisq.intermediate(p, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
df.intermediate(x, df1, df2, ncp=0, log=FALSE)
pf.intermediate(q, df1, df2, ncp=0, lower.tail=TRUE, log.p=FALSE)
qf.intermediate(p, df1, df2, ncp=0, lower.tail=TRUE, log.p=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x,p,q, df,df1,df2, ncp, log,log.p, lower.tail}{See
  \code{\link{pchisq}} and \code{\link{pf}}.  Some arguments
  don't exist in S-Plus.  That is why these functions are needed.
}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{misc}
