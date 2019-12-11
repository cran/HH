\name{mmcPruneIsomeans}
\alias{mmcPruneIsomeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MMC plots in lattice---suppress isomeans grid lines for specified levels
of the factor.
}
\description{
MMC plots in lattice---suppress isomeans grid lines for specified levels
of the factor.
}
\usage{
mmcPruneIsomeans(mmc, keep=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mmc}{
An \code{"mmc.multicomp"} object.
}
  \item{keep}{
Index vector of rows of \code{mmc$none$table} that will be kept in the display.
}
}
\value{
A modified \code{"mmc.multicomp"} object.
}

\seealso{
\code{\link{mmc}}
}
\examples{
## needed
\dontrun{
## See file hh/scripts/hh2/tway.R for the complete example.
## A better example is needed for the .Rd documentation.
## possibly based on filmcoat temperature | pressure example.
data(rhiz.clover)
c(1,2,5,10,11,12)
rhiz.clover$cs <- with(rhiz.clover, interaction(comb, strain))
rhiz.clover.cs.aov <- aov(Npg ~ cs, data=rhiz.clover)
rhiz.clover.cs.aov
cs.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
              calpha=qtukey( .95, 6,  48)/sqrt(2))
dlmat2 <- dimnames(cs.mmc$mca$lmat)[[2]]
cl.index <- grep("clover\\\\.[[:print:]]*clover\\\\.", dlmat2, value=TRUE)
%% cl.index <- grep("clover\\.[[:print:]]*clover\\.", dlmat2, value=TRUE)
cl.index
clover.lmat <- cs.mmc$mca$lmat[, cl.index] ## suppress "clover+alfalfa" contrasts
dimnames(clover.lmat)[[1]]
dimnames(clover.lmat)[[1]] <- levels(rhiz.clover$cs)
clover.lmat[1,] <- -colSums(clover.lmat[-1, ])
clover.lmat
csc.mmc <- mmc(rhiz.clover.cs.aov, linfct=mcp(cs="Tukey"),
               focus.lmat=clover.lmat,
               calpha=qtukey( .95, 6,  48)/sqrt(2))
## this example needs a window 11 inches high and 14 inches wide
mmcplot(csc.mmc, type="lmat", style="both")

## suppress "clover+alfalfa" means
csc.mmc.clover <- mmcPruneIsomeans(csc.mmc, keep = c(1,2,5,10,11,12))
csc.mmc.clover
## this example needs a window 11 inches high and 14 inches wide
mmcplot(csc.mmc.clover, type="lmat", style="both")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
