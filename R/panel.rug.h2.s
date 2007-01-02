## H2 function by rmh
## This function is for S-Plus only.  It is not for R.
## panel.rug is to be donated to Insightful.
##
## S-Plus scoping during package installation makes it necessary to
## define the function for both languages and then remove it from R.
## S-Plus scoping during installation won't allow function definition
## inside the if.R() statement.  Therefore we always assign the
## function, and then remove it from the R package.


panel.rug <- function (x = NULL,
                       ticksize = 0.03, side = 1, lwd = 0.1,
                       col=1, ...) {
  x <- as.vector(x)
  col <- rep(col, length=length(x))
  for (i in seq(along=x))
    axis(side=side, at = x[i], lab = F, lwd = lwd, fg=col[i],
         tck=ticksize, ...)
}

## Remove it from the R package.
##
if.R(s={},
     r={rm(panel.rug)})
