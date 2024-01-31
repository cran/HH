
"if.R" <-
function(r, s) {
  if (exists("is.R") && is.function(is.R) && is.R()) r
  else s
}

## copied from earlier R.  Now deprecated, soon to be defunct
is.R <- function ()
exists("version") && !is.null(vl <- version$language) && vl == "R"
