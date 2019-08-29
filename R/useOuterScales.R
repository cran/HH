if (getRversion() >= '2.15.1') globalVariables('ALR')

useOuterScales <-
  function(x,
           axis.xlab.padding=4,
           ylab.axis.padding=3,
           strip,
           strip.left,
           layout.widths.strip.left=.5,
           layout.heights.strip=.5,
           x.ticks=is.numeric(x$x.limits),
           y.ticks= is.numeric(x$y.limits) +
             if (!missing(strip.left) &&   ## FALSE
                 is.logical(strip.left) && ## explicitly stated
                 !strip.left ) 0
             else 2.5,
           inner=FALSE,
           interchangeRC=FALSE)
{
  as.table <- x$as.table

## ------------------------------------------------------------------------------------------
## strip (argument)         inner     interchangeRC   as.table     strip (generated)
## ------------------------------------------------------------------------------------------
## missing(strip) || TRUE   FALSE     FALSE           FALSE        strip.useOuterStrips.last
## missing(strip) || TRUE   FALSE     FALSE           TRUE         strip.useOuterStrips.first
## missing(strip) || TRUE   FALSE     TRUE                         error
## missing(strip) || TRUE   TRUE      FALSE                        strip.top1
## missing(strip) || TRUE   TRUE      TRUE                         strip.top2
## specified || FALSE                                              ## input
## ------------------------------------------------------------------------------------------

  if (missing(strip)  || (is.logical(strip) && strip)) {

    if (!inner && !interchangeRC) {
      strip <- if (!as.table)
                 strip.useOuterStrips.last
               else
                 strip.useOuterStrips.first
    }

    if (!inner && interchangeRC)
      stop('(!inner && interchangeRC) are incompatible', call.=FALSE)

    if (inner && !interchangeRC)
      strip <- strip.top1

    if (inner && interchangeRC)
      strip <- strip.top2
  }
  ## Valid strip argument values that fall through are the name of a
  ## properly constructed strip function or FALSE.  Anything else will
  ## generate an error later.


  ## -------------------------------------------------------------------------------
## strip (argument)             inner    interchangeRC   strip.left (generated)
## -------------------------------------------------------------------------------
## missing(strip.left) || TRUE  FALSE    FALSE           strip.left.useOuterStrips
## missing(strip.left) || TRUE  FALSE    TRUE            error
## missing(strip.left) || TRUE  TRUE     FALSE           strip.left2
## missing(strip.left) || TRUE  TRUE     TRUE            strip.left1
## specified || FALSE                                    ## input
## -------------------------------------------------------------------------------

  if (missing(strip.left) || (is.logical(strip.left) && strip.left)) {

    if (!inner && !interchangeRC)
      strip.left <- strip.left.useOuterStrips

    if (!inner && interchangeRC)
      stop('(!inner && interchangeRC) are incompatible', call.=FALSE)
    ## This is redundant with the similar line above

    if (inner && !interchangeRC)
      strip.left <- strip.left2

    if (inner && interchangeRC)
      strip.left <- strip.left1

  }
  ## Valid strip.left argument values that fall through are the name
  ## of a properly constructed strip function or FALSE.  Anything else
  ## will generate an error later.


  AtLabelsRot <- function(z.scales, z.limits) {
    z.scales <- z.scales[c("at", "labels", "rot")]
    if (is.numeric(z.limits)) {
      if (!is.numeric(z.scales$at)) {
        z.scales$at <- pretty(range(z.limits))
        z.scales$labels <- TRUE
      }
    }
    else {
      limits <- if (is.list(z.limits))
                  z.limits[[1]]
                else
                  z.limits
      z.scales$at <- seq(along=limits)
      z.scales$labels <- limits
    }
    z.scales
  }


  update(x,
         strip=strip,
         strip.left=strip.left,
         par.settings=list(
           clip=list(panel="off"),
           layout.widths=list(ylab.axis.padding=ylab.axis.padding,
                              strip.left=layout.widths.strip.left),
           layout.heights=list(axis.xlab.padding=axis.xlab.padding,
                               strip=layout.heights.strip)),
         scales=list(x=list(draw=FALSE),
                     y=list(draw=FALSE))
         ) +

  layer(if (current.row()==(if (x$as.table) ncol(x) else 1)) ## "trellis"-object row
          do.call(panel.axis,
                  c(list("bottom", outside=TRUE, ticks=x.ticks), ALR)),
        data=list(x=x, x.ticks=x.ticks, ALR=AtLabelsRot(x$x.scales, x$x.limits))) +

  layer(if (current.column()==1) ## "trellis"-object col
          do.call(panel.axis,
                  c(list("left", outside=TRUE, ticks=y.ticks), ALR)),
        data=list(x=x, y.ticks=y.ticks, ALR=AtLabelsRot(x$y.scales, x$y.limits)))
}


strip.top2 <-
function (which.given, which.panel, var.name, ...)
{
        strip.default(which.given = 1, which.panel = which.panel[2],
            var.name = var.name[1], ...)
}

strip.top1 <-
function (which.given, which.panel, var.name, ...)
{
  if (which.given==1)
        strip.default(which.given = 1, which.panel = which.panel[1],
            var.name = var.name[1], ...)
}

strip.left1 <-
function (which.given, which.panel, var.name, ...)
{
  if (which.given==1)
        strip.default(which.given = 1, which.panel = which.panel[1],
            var.name = var.name[1], ...)
}

strip.left2 <-
function (which.given, which.panel, var.name, ...)
{
  if (which.given==2)
        strip.default(which.given = 1, which.panel = which.panel[2],
            var.name = var.name[2], ...)
}


strip.useOuterStrips.first <-
function (which.given, which.panel, var.name, ...)
{
    row.to.keep <- 1
    if (which.given == 1 && current.row() == row.to.keep)
        strip.default(which.given = 1, which.panel = which.panel[1],
            var.name = var.name[1], ...)
}

strip.useOuterStrips.last <-
function (which.given, which.panel, var.name, ...)
{
    row.to.keep <- nrow(trellis.currentLayout())
    if (which.given == 1 && current.row() == row.to.keep)
        strip.default(which.given = 1, which.panel = which.panel[1],
            var.name = var.name[1], ...)
}

strip.left.useOuterStrips <-
function (which.given, which.panel, var.name, ...)
{
    if (which.given == 2 && current.column() == 1)
        strip.default(which.given = 1, which.panel = which.panel[2],
            var.name = var.name[2], ...)
}

