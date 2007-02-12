plot.multicomp.hh <-
  function(x, ylabel = x$ylabel, href = 0, uniform = T,
           plt.in = c(0.2, 0.9, 0.1, 0.9),
           ##         fig.in=par("fig"),
           x.label.adj=1,
           xrange.include=href,
           xlim,
           comparisons.per.page=21,
           col.signif=1, col.not.signif=1,
           lty.signif=4, lty.not.signif=4,
           lwd.signif=1, lwd.not.signif=1,
           ...,
           xlabel.print=TRUE)
{
  if.R(r=
       NextMethod("plot")
       ,
       s={
         table <- x$table
         method <- x$method
         if(method == "lsd")
           method <- "LSD"
         if(method == "scheffe")
           method <- "Scheffe"
         if(method == "bon")
           method <- "Bonferroni"
         if(method == "sidak")
           method <- "Sidak"
         if(method == "tukey")
           method <- "Tukey"
         if(method == "dunnett")
           method <- "Dunnett"
         if(method == "sim")
           method <- "simulation-based"
         if(method == "tpmc")
           method <- "Cheung and Chan"
         if(x$error.type == "fwe")
           xlabel <- paste("simultaneous ", as.character(100 * (1 - x$
                                                                alpha)), "% confidence limits,", method, "method")
         if(x$error.type == "cwe")
           xlabel <- paste("individual", as.character(100 * (1 - x$alpha)),
                           "% confidence limits,", method, "method")

         old.par <- par()       ## this is imperative.
         ##  There is a bug in plt through at least S-Plus 7.0.6
         on.exit({par(old.par); par(new=FALSE); par(new.usr)})  ## this is imperative.
         old.plt <- par(plt = plt.in)
         ##        par(plt = plt.in, fig=fig.in)
         ##        old.par <- par()      ## at least one other par value is affected
         ##        par(new=F)            ## new seems to be reset when fig is set
         ##        on.exit({par(old.par)
         ##                 par(new=F)
         ##                 par(new.usr)}) ## restore everything except new and usr

         ptot <- nrow(table)
         cpp <- comparisons.per.page
         npage <- ceiling(ptot/cpp)
         for(page in 1:npage) {
           estimate <- table[((page - 1) * cpp + 1):min(((page - 1) * cpp +
                                                         cpp), ptot), "estimate"]
           lower <- table[((page - 1) * cpp + 1):min(((page - 1) * cpp + cpp),
                                                     ptot), "lower"]
           upper <- table[((page - 1) * cpp + 1):min(((page - 1) * cpp + cpp),
                                                     ptot), "upper"]
           labels <- dimnames(table)[[1]][((page - 1) * cpp + 1):min(((
                                                                       page - 1) * cpp + cpp), ptot)]
           signif <- (lower * upper > 0) ## hh
           p <- length(estimate)
           lower <- ifelse(lower == ( - Inf), NA, lower)
           upper <- ifelse(upper == Inf, NA, upper)
           if (missing(xlim)) {
             if(page == 1 || !uniform) {
               xmin <- min(c(lower, upper, estimate, href,
                             xrange.include), na.rm = T)
               xmax <- max(c(lower, upper, estimate, href,
                             xrange.include), na.rm = T)
               xrange <- range(pretty(c(xmin, xmax), 10))
             }}
           else {
             xmin <- xlim[1]
             xmax <- xlim[2]
             xrange <- range(pretty(c(xmin, xmax), 10))
           }
           lines.lower <- ifelse(is.na(lower), min(xrange), lower)
           lines.upper <- ifelse(is.na(upper), max(xrange), upper)
           ends.lower <- !is.na(lower)
           ends.upper <- !is.na(upper)
           heights <- cpp:(cpp - p + 1)
           plot(estimate, heights, ylab = "", ylim = c(0, cpp+1), yaxt = "n",
                xlim = xrange, xlab = "", xaxt = "n", pch = 16, bty = 
                "n", ...,
                type="n")
           new.usr <- par()["usr"]
           if (sum(signif))
             points(estimate[signif], heights[signif], pch=16, col=col.signif)
           if (sum(!signif))
             points(estimate[!signif], heights[!signif], pch=16, col=col.not.signif)

           xlower <- lower[ends.lower & signif]
           if(length(xlower) > 0)
             points(xlower, heights[ends.lower & signif], pch = "(",
                    col=col.signif)
           xupper <- upper[ends.upper & signif]
           if(length(xupper) > 0)
             points(xupper, heights[ends.upper & signif], pch = ")",
                    col=col.signif)
           segments(lines.lower[signif], heights[signif],
                    lines.upper[signif], heights[signif],
                    lty = lty.signif, lwd = lwd.signif,
                    col=col.signif)

           xlower <- lower[ends.lower & !signif]
           if(length(xlower) > 0)
             points(xlower, heights[ends.lower & !signif], pch = "(",
                    col=col.not.signif)
           xupper <- upper[ends.upper & !signif]
           if(length(xupper) > 0)
             points(xupper, heights[ends.upper & !signif], pch = ")",
                    col=col.not.signif)
           segments(lines.lower[!signif], heights[!signif],
                    lines.upper[!signif], heights[!signif],
                    lty = lty.not.signif, lwd = lwd.not.signif,
                    col=col.not.signif)

           if(!is.null(href))
             segments(href, rep(cpp+1 - p - 1, length(href)), href,
                      rep(cpp+1, length(href)))
           if (sum(signif))
             text(xmin - 0.1 * (xmax - xmin),
                  heights[signif], labels[signif], col=col.signif,
                  adj = x.label.adj)
           if (sum(!signif))
             text(xmin - 0.1 * (xmax - xmin),
                  heights[!signif], labels[!signif], col=col.not.signif,
                  adj = x.label.adj)
           axis(1, at = pretty(c(xmin, xmax), 10), pos = cpp - p)
           if (xlabel.print) {
             text((xmax + xmin)/2, (cpp - p - 3)*(22/cpp)-(cpp/22),
                  xlabel, adj = 0.5)
             text((xmax + xmin)/2, (cpp - p - 3)*(22/cpp)-2*(cpp/22),
                  paste("response variable:", ylabel), adj = 0.5)
           }
           lines(c(xrange[1], xrange[1]), c(cpp - p, cpp+1))
           lines(c(xrange[2], xrange[2]), c(cpp - p, cpp+1))
           lines(c(xrange[1], xrange[2]), c(cpp+1, cpp+1))
           lines(c(xrange[1], xrange[2]), c(cpp - p, cpp - p))
         }
       }
       )
}