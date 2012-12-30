
axis.i2wt.new <-
  function(side, scales, ...)
{
  tcL <- trellis.currentLayout()
  rows.per.page <- dim(tcL)[1]
  cols.per.page <- dim(tcL)[2]
  cell <- panel.number()
  row.panel <- row(tcL)[tcL==cell]
  column.panel <- col(tcL)[tcL==cell]

  ## if (side == "bottom" && row.panel != 1) return()
  axis.default(side = side, scales = scales, ...)
}

