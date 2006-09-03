"lm.case" <-
function(fit, lms=summary.lm(fit), lmi=lm.influence(fit)) {
  e <- fit$residuals
  s <- lms$sigma
  xxi <- diag(lms$cov.unscaled)
  si <- lmi$sigma
  h <- lmi$hat
if.R(s=
  bi <-  t(coef(fit) - t(coef(lmi)))  ## corrected!  line in CH p130 is wrong.
,r=
  bi <-  coef(lmi)                    ## R did the subtraction
)
  sta.res <- e/(s*(1-h)^.5)
  stu.res <- e/(si*(1-h)^.5)  ## uses si, not s
  dfbetas <- bi/(si %o% xxi^.5)
  dimnames(dfbetas)[[2]] <- names(fit$coefficients)
  dffit <- h*e/(1-h)
  dffits <- h^.5*e/(si*(1-h))
  cook <- sta.res^2/length(xxi) * h/(1-h)
  tmp <- cbind(e, h, si, sta.res, stu.res, dffit, dffits, cook, dfbetas)
  if(is.null(fit$na.action))
    return(tmp)
  else
    return(naresid(fit$na.action, tmp))
}

## plot.case is based on:
## Section 4.3.3 Influence of Individual Obervations
## in Chambers and Hastie, Statistical Models in S.
"plot.case" <- function(x, fit,
                        which=c("stu.res","si","h","cook","dffits",
                          dimnames(x)[[2]][-(1:8)]),  ##DFBETAS
                        between.in=list(y=4, x=9),
                        oma=c(0,0,0,4), cex.threshold=if.R(s=2, r=1.2),
                        main.in=list(
                          paste(deparse(fit$call), collapse=""),
                          cex=main.cex),
                        sigma.in=summary.lm(fit)$sigma,
                        p.in=summary.lm(fit)$df[1]-1,
                        obs.large=".lm.case.large",
                        obs.large.env=if.R(r=globalenv(), s=0),
                        main.cex=NULL,
                        ...) {
  p <- p.in
  n <- dim(x)[1]

  ncs <- dimnames(x)[[2]]

  ncs.keep <- if (is.numeric(which)) which else match(which, ncs, 0)

  ncs[-(1:8)] <- paste("DFBETAS", ncs[-(1:8)])
  ncs[match("si",ncs)] <- "deleted std dev"
  ncs[match("cook",ncs)] <- "Cook's distance"
  ncs[match("stu.res",ncs)] <- "Student del resid"
  case.data.frame <-
    data.frame(y     = as.vector(unlist(x[,ncs.keep])),
               id    = rep(1:n, length(ncs.keep)),
               group = factor(
                 rep(ncs[ncs.keep], rep(n, length(ncs.keep))),
                 levels=ncs[ncs.keep]))
  case.data.frame$rownames <- rep(dimnames(x)[[1]],
                                  length(ncs.keep))
  ## 'rownames' is a character variable.  This is not 'row.names'.
  if.R(s={old.oma <- par(oma=oma)
          warning(paste('par("oma") has been reset from\n',
                        'par(oma=', deparse(as.integer(old.oma$oma)), ')\n',
                        sep=""))
        },
       r={})
  scales.y <- list(relation="free")
  if.R(r=scales.y["at"] <- list(NULL),
       s=scales.y$yaxt <- "n")
  xyplot(y ~ id | group, data=case.data.frame,
         subscripts=TRUE,
         rownames=case.data.frame$rownames,
         group.names=levels(case.data.frame$group),
         panel=panel.case,
         main=main.in,
         xlab="",
         ylab="",
         between=between.in,
         as.table=TRUE,
         scales=list(
           y=scales.y,
           x=list(alternating=2, tck=-.005)),
         nn=n, pp=p, ss=sigma.in,
         cex.threshold=cex.threshold,
         par.settings = list( ## R only.  Ignored by panel.case in S-Plus
           ##clip = list(panel = FALSE),
           layout.widths = list(axis.key.padding = 16)),
         obs.large=obs.large, obs.large.env=obs.large.env,
         ...)
}



"panel.case" <- function(x, y, subscripts, rownames, group.names,
                         nn, pp, ss, cex.threshold,
                         panel.number, ## R only. S-Plus ignores this argument
                         par.settings, ## R only. S-Plus ignores this argument
                         obs.large, obs.large.env,
                         ...) {
  cell.num <- if.R(s=get("cell", frame=sys.parent()),
                   r=panel.number)
  panel.label <- if.R(s=get("panel.labels", frame=sys.parent())[cell.num],
                      r=group.names[cell.num])
  cex.x <- if.R(s=get("cex.x", frame=sys.parent()),
                r=trellis.par.get("axis.text")$cex)
  cex.y <- if.R(s=get("cex.y", frame=sys.parent()),
                r=cex.x)
  y.plot <- y
  pretty.y <- pretty(y)
  lin.pretty.y <- pretty.y
  new.viewport <- FALSE
  switch(panel.label,
         "deleted std dev"={y.plot <- y-ss
                            lin.pretty.y <- pretty.y-ss
                            if.R(s={par.usr <-par()$usr
                                    par(usr=c(par.usr[1:2], par.usr[3:4]-ss))},
                                 r={new.viewport <- TRUE
                                    cvp <- current.viewport()
                                    cvp$yscale <- cvp$yscale - ss
                                    pushViewport(cvp)
                                  })
                            threshold <- c(.95*ss,ss,1.05*ss)-ss
                            thresh.label <- c(".95 s","s","1.05 s")
                            thresh.id <- c(.95*ss, 1.05*ss)
                          },
         e={threshold <- c(-3*ss,-2*ss,0,-ss,ss,2*ss,3*ss)
            thresh.label <- c("-3s","-2s","0","-s","s","2s","3s")
            thresh.id <- c(-2*ss, 2*ss)
          },
         h={pretty.y <- pretty(c(0,y))
            lin.pretty.y <- pretty.y
            if.R(s={par.usr <- par()$usr
                    par(usr=c(par.usr[1:2], 0, par.usr[4]))},
                 r={new.viewport <- TRUE
                    cvp <- current.viewport()
                    cvp$yscale[1] <- 0
                    pushViewport(cvp)
                  })
            threshold <- c(0, 1/nn, c(2,3) * (pp+1)/nn)
            thresh.label <- c("0", "1 / n", "2 (p+1)/n", "3 (p+1)/n")
            thresh.id <- c(0, 2*(pp+1)/nn)
          },
         "Cook's distance"={if.R(s={par.usr <-par()$usr
                                    par(usr=c(par.usr[1:2], 0, max(1.05, y)))},
                                 r={new.viewport <- TRUE
                                    cvp <- current.viewport()
                                    cvp$yscale <- c(0, max(1.05, y))
                                    pushViewport(cvp)
                                  })
                            pretty.y <- if.R(s=pretty(par()$usr[3:4]),
                                             r=pretty(cvp$yscale))
                            lin.pretty.y <- pretty.y
                            threshold <- c(-1,0,1)
                            thresh.label <- c("","0","1")
                            thresh.id <- c(-1,1)
                          },
         ## ## if we ever want to restore the 4/(nn-pp-1) criterion
         ## "Cook's distance"={threshold <- c(-1,0,4/(nn-pp-1), 1)
         ##                    thresh.label <- c("","0","4 / (n-p-1)", "1")
         ##                    thresh.id <- c(-1, 4/(nn-pp-1))
         ##                  },
         dffits={threshold <- c(-2,0,2)*sqrt(pp/nn)
            thresh.label <- c("-2 sqrt(p/n)", "0", "2 sqrt(p/n)")
            thresh.id <- c(-2,2)*sqrt(pp/nn)
          },
         sta.res=,
         "Student del resid"={threshold <- c(-3,0,3)
            thresh.label <- c(-3,0,3)
            thresh.id <- c(-3,3)
          },
         dffit={threshold <- c(-100,0,100) ##arbitrary large numbers
            thresh.label <- c(-100,0,100)
            thresh.id <- c(-100,100)
          },
         ## DFBETAS
         {threshold <- c(-2,0,2)/sqrt(nn)
          thresh.label <- c("-2 / sqrt(n)", "0", "2 / sqrt(n)")
          thresh.id <- c(-2,2)/sqrt(nn)
        }
         )

    panel.xyplot(x, y.plot, type="h", ...)

  if (length(lin.pretty.y) > 0)
    if.R(s=      axis(side=2,      at=lin.pretty.y, labels=pretty.y,      cex=1.5*cex.y, adj=1),
         r=panel.axis(side="left", at=lin.pretty.y,
           labels=pretty.y, text.cex=1.0*cex.y, outside=TRUE)
         )

##   if.R(r={
##     cpv <- current.viewport()
##     cpv$clip <- FALSE
##     pushViewport(cpv)
##   },
##        s={})
  if.R(r={
    yscale <- current.viewport()$yscale
    thresh.inside.yscale <- (yscale[1] < threshold) &  (yscale[2] > threshold)
    threshold <- threshold[thresh.inside.yscale]
  }, s={})
  panel.abline(h=threshold, lty=2, err=-1)
##   if.R(r=popViewport(),
##        s={})

  if (length(threshold) > 0)
    if.R(s=      axis(side=4,       at=threshold, labels=thresh.label, err=-1,
                cex=cex.threshold*cex.y, tck=-.02, adj=0),
         r=panel.axis(side="right", at=threshold, labels=thresh.label,
           text.cex=cex.threshold*cex.y, tck=-.02, outside=TRUE))
  y.compare <- (y < thresh.id[1]) | (y > thresh.id[2])
  y.compare[is.na(y.compare)] <- FALSE
  subs <- (1:nn)[y.compare]
  ## use the for() loop to force all items to print
  names.x <- rownames[subscripts]
  if (length(subs) > 0)
    for (i in subs)
      if.R(s=      axis(side=1,        at=i, labels=names.x[i],      cex=1.7*cex.x,
             tick=TRUE, tck=-.06),
           r=panel.axis(side="bottom", at=i, labels=names.x[i], text.cex=1.2*cex.x,
             tick=TRUE, tck=-.06, outside=TRUE, rot=0))
  if (new.viewport) popViewport()

  ## save the row.names of the observations crossing the threshold.
  if (!exists(obs.large, obs.large.env))
    assign(obs.large, list(), obs.large.env)
  tmp <- get(obs.large, obs.large.env)
  tmp[[panel.label]] <- names.x[subs]
  assign(obs.large, tmp, obs.large.env)
}
