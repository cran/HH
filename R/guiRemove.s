### remove graph sheet pages, equivalent to right-click delete on the tab.
###  GSremove(c(3,4,5,6,8,9,12,13))

GSremove <- function(pages, sheet="GSD2$Page") {
  if.R(s=
       for (i in paste(sheet, pages, sep=""))
       guiRemove( "GraphSheetPage", Name = i, ShiftLeft = F)
       ,r=warning("No-op function in R.")
       )
}
