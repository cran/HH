## demo/ancova.r

script_HH <- function(scriptname) {
  tmp <- search()
  HH.index <- match("package:HH", tmp, 0)
  HH.file <- paste(searchpaths()[HH.index], scriptname, sep="/")
  HH.file
}

cat("Please see the last graph produced by the script Ch10-regbb.r in file\n")
cat(script_HH("Ch10-regbb.r"), "\n")
cat("This is a composite graph illustrating four models with a factor and a covariate.\n")
cat("\n")
cat("y ~ x                        ## linear regression                              \n")
cat("y ~ g                        ## analysis of variance                           \n")
cat("y ~ g + x  ## or y ~ x + g   ## analysis of covariance                         \n")
cat("y ~ g * x  ## or y ~ x * g   ## analysis of covariance with an interaction term\n")
cat("\n")
cat("y ~ x                        ## constant intercept, constant slope\n")
cat("y ~ g                        ## variable intercept,     zero slope\n")
cat("y ~ g + x  ## or y ~ x + g   ## variable intercept, constant slope\n")
cat("y ~ g * x  ## or y ~ x * g   ## variable intercept, variable slope\n")

