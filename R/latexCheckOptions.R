latexCheckOptions <- function(...) {
if (any(sapply(options()[c("latexcmd","dviExtension","xdvicmd")], is.null)))
  stop("This example uses the pdflatex system command and R's pdf() graphics\n",
       "device and therefore requires that the three options\n",
       "      options()[c(\"latexcmd\",\"dviExtension\",\"xdvicmd\")]\n",
       "all be set to non-NULL values.\n",
       "    latexSetOptions()\n",
       "defines a consistent set of options. Please see the comments in the \"Details\"\n",
       "section of ?Hmisc::latexCheckOptions for some recommendations, and the\n",
       "\"System options\" paragraph in the \"Details\" section of ?Hmisc::latex\n",
       "for full discussion of the options available and suggested values for\n",
       "several operating systems.  If you wish to use the latex system command\n",
       "and a compatible graphics device, see the discussion in ?Hmisc::latex",
       call.=FALSE)
}

latexSetOptions <-
  function(latexcmd=c("pdflatex", "latex"),
           dviExtension={
             if (is.null(latexcmd)) NULL
             else
               switch(latexcmd,
                      pdflatex="pdf",
                      latex="dvi")
           },
           xdvicmd={
             if (is.null(latexcmd)) NULL
             else
               switch(latexcmd,
                      pdflatex=if (nchar(Sys.which("open")))
                                 "open"      ## Macintosh, Windows, SMP linux
                               else
                                 "xdg-open", ## ubuntu linux
                      latex="dvips") ##
                                     ## dvips  Mac, Win: .ps in wd displayed
                                     ## xdvi   Mac: Quartz displays image borders
                                     ##             and waits until dismissed.
                                     ## xdvi   Windows: not on my machine.
                                     ## yap    Windows: dvi is displayed
                                     ## open   Mac: nothing happens
                                     ## open   Windows: yap displays dvi
           }
           ) {
    if (!is.null(latexcmd)) latexcmd <- match.arg(latexcmd)
    if (!is.null(latexcmd) && latexcmd == "latex") {
      cat("We recommend 'latexcmd=\"pdflatex\"'.\n",
          "  With 'latex', ps files are not in color\n",
          "  and png files need additional bb (bounding box) information.\n",
          "  With 'latex', remember to use 'device=\"postscript\"' or 'device=\"png\"'\n",
          "  in latex() and microplot().\n")
    }
    ## set options
    options(latexcmd=latexcmd,
            dviExtension=dviExtension,
            xdvicmd=xdvicmd)
    ## return options as set by this call (NOT the previous settings)
    invisible(c(options("latexcmd"), options("dviExtension"), options("xdvicmd")))
  }
