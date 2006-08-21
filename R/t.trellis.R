"t.trellis" <-
function(x) {
  warning(paste("t.trellis is an identity operator in R.\n",
                "Please change the formula from (y ~ x | g) to (x ~ y | g)\n"))
  x
}

