ReadBeforePackageToSPlus <- function(datasetName, hhFileName,
                                     read=TRUE, example=FALSE, hh="hh.file") {
  if (exists(datasetName)) return()
  stop(paste('Please use ', hh, '("', hhFileName, '")',
             if (read) " to read the data",
             if (read && example) " and",
             if (example) " for examples of the analysis." else ".",
             sep=""),
       call.=FALSE)
}
