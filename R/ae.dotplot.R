## variable names in the input data.frame ae
##
## RAND   treatment as randomized
## PREF   adverse event symptom name
## SN     number of patients in treatment group
## SAE    number of patients  in each group for whom the event PREF was observed
##
## Input sort order is PREF/RAND

## Calculate the percent, the relative risk, the log relative risk,
## and the confidence intervals.
## Make PREF an ordered factor, sorted by the relative risk.
logrelrisk <- function(ae, A.name, B.name, crit.value=1.96) {
  ae$PCT <- 100 * ae$SAE / ae$SN ## percent of patients
  tmp <-  ## sample relative risk
    ae$PCT[ae$RAND==B.name] /
      ae$PCT[ae$RAND==A.name]

  ## sort by relrisk
  tmp.order <- order(tmp) ## sort by logrelrisk
  ##  ordered(ae$PREF) <- as.character(unique(ae$PREF))[tmp.order]  ## S-Plus only
  ae$PREF <- ordered(ae$PREF, levels=(ae$PREF[ae$RAND==A.name])[tmp.order]) ## R and S-Plus
  
  ae$relrisk <- as.vector(rbind(tmp,tmp))
  ae$logrelrisk <- log(ae$relrisk)
  
  ase.logrelrisk <-
    ## sample asymptotic standard error of relative risk
    ##  (Agresti, Equation 3.18)
    sqrt((1-ae$PCT[ae$RAND==B.name]/100) /
         (ae$PCT[ae$RAND==B.name]/100 *
          ae$SN[ae$RAND==B.name])
         + (1-ae$PCT[ae$RAND==A.name]/100) /
         (ae$PCT[ae$RAND==A.name]/100   *
          ae$SN[ae$RAND==A.name]  ))
  ae$ase.logrelrisk <- as.vector(rbind(ase.logrelrisk, ase.logrelrisk))
  
  ae$logrelriskCI.lower <- ae$logrelrisk - crit.value*ae$ase.logrelrisk
  ae$logrelriskCI.upper <- ae$logrelrisk + crit.value*ae$ase.logrelrisk
  ae$relriskCI.lower <- exp(ae$logrelriskCI.lower)
  ae$relriskCI.upper <- exp(ae$logrelriskCI.upper)

  ae
}


panel.ae.leftplot <- function(x, y, groups, col.AB, ...) {
  panel.abline(h=y, lty=2, lwd=0, col=1)
  panel.superpose(x, y, groups=groups, col=col.AB, ...)
}

panel.ae.rightplot <- function(x, y, ..., lwd=6, lower, upper) {
  if.R(r={}, s={panel.segments <- segments; panel.points <- points})
  panel.abline(v=0, lty=3, lwd=0)
  panel.abline(h=y, lty=2, lwd=0, col=1)
  panel.segments(lower, y, upper, y, lwd=2)
  panel.xyplot(x, y, ..., col=1, cex=.7)
  panel.points(lower, y, pch=3, col=1, cex=.4)
  panel.points(upper, y, pch=3, col=1, cex=.4)
}

panel.ae.dotplot <- function(x, y, groups, ..., col.AB, pch.AB, lower, upper) {
  panel.num <- if.R(s=get("cell", frame=sys.parent()),
                    r=panel.number())
  if (panel.num==1)
    panel.ae.leftplot(x, y, groups=groups, col=col.AB, pch=pch.AB, ...)
  if (panel.num==2)
    panel.ae.rightplot(x, y, ..., lwd=6, pch=16,
                       lower=lower, upper=upper)
}


ae.dotplot <- if.R(r={
  function(xr,
           A.name=levels(xr$RAND)[1],
           B.name=levels(xr$RAND)[2],
           col.AB=c("red","blue"), pch.AB=c(16,17),
           main.title="Most Frequent On-Therapy Adverse Events Sorted by Relative Risk",
           main.cex=1,
           cex.AB.points=NULL, cex.AB.y.scale=.6,
           position.left= c(0,   0, .70, 1.), ## ignored in R
           position.right=c(.61, 0, .98, 1.), ## ignored in R
           key.y=-.2, CI.percent=95) {
    
    result <-
      dotplot(PREF ~ PCT + logrelrisk,
              groups=xr$RAND, data=xr, outer=TRUE,
              lower=xr$logrelriskCI.lower,
              upper=xr$logrelriskCI.upper,
              panel=panel.ae.dotplot,
              scales=list(
                x=list(
                  relation="free",
                  at=list(
                    seq(0, 100, 10),
                    log(c(.125, .25, .5, 1, 2, 4, 8, 16, 32))
                    ),
                  labels=list(
                    seq(0, 100, 10),
                    c(".125", "", ".5", 1, 2, 4, 8, 16, 32)
                    ),
                  limits=list(
                    range(xr$PCT),
                    range(xr$logrelriskCI.lower, xr$logrelriskCI.upper))
                  ),
                y=list(cex=cex.AB.y.scale)),
              A.name=A.name, B.name=B.name,
              col.AB=col.AB, pch.AB=pch.AB,
              cex.AB.points=cex.AB.points,
              cex.AB.y.scale=cex.AB.y.scale,
              main=list(main.title, cex=main.cex),
              xlab=NULL,
              between=list(x=1),
              key=list(y = key.y, x=.15,
                points = list(col=col.AB, pch=pch.AB),
                text = list(c(A.name, B.name), col=col.AB, cex=.9),
                columns = 2,
                between=.5,
                space="bottom")
              )
    if.R(r=result$condlevels[[1]] <-
         c("Percent",
           paste("Relative Risk with ", CI.percent, "% CI", sep=""))
         )
    result
  }
},s={

  function(xr,
           A.name=levels(xr$RAND)[1],
           B.name=levels(xr$RAND)[2],
           col.AB=c("red","blue"), pch.AB=c(16,17),
           main.title="Most Frequent On-Therapy Adverse Events Sorted by Relative Risk",
           main.cex=1,
           cex.AB.points=NULL, cex.AB.y.scale=.6,
           position.left= c(0,   0, .70, 1.),
           position.right=c(.61, 0, .98, 1.),
           key.y=-.2, CI.percent=95) {

    ae.key <- list(y = -.3,
                   points = list(col=0),
                   text = list(""),
                   columns = 2,
                   space="bottom")
    
    ae.main <- list(" ", cex=1.5)
    
    ## construct left panel
    left.plot <- dotplot(PREF ~ PCT, data=xr, groups = RAND,
                         col=col.AB, pch=pch.AB,
                         panel = panel.ae.leftplot,
                         xlab = "Percent",
                         scales=list(y=list(cex=cex.AB.y.scale)),
                         main = ae.main,
                         cex=cex.AB.points,
                         key = ae.key
                         )

    ## construct right panel
    right.plot <- dotplot(PREF ~ logrelrisk, data=xr, pch=16,
                          lower=xr$logrelriskCI.lower,
                          upper=xr$logrelriskCI.upper,
                          panel=panel.ae.rightplot,
                          xlab = paste("Relative Risk with ",
                            CI.percent, "% CI", sep=""),
                          scales=list(
                            x=list(at=log(c(.125, .25, .5, 1, 2, 4, 8, 16, 32)),
                              labels=c(".125", "", ".5", 1, 2, 4, 8, 16, 32)),
                            y=list(cex=cex.AB.y.scale)
                            ),
                          xlim=c(-2.3,4),
                          main = ae.main,
                          key = ae.key
                          )
    ##  right.plot
    if.R(s=
         right.plot$scales$y$labels[] <- " " ## suppress the AE names on the right panel
         ,r=
         right.plot$y.limits[] <- ""
         )
    ##  right.plot

    ## print both plots on current device
    print(left.plot, position=position.left,  more=TRUE)
    print(right.plot, position=position.right, more=FALSE)
    title(main.title, cex=main.cex)
    key(y = key.y, x=.15,
        points = list(col=col.AB, pch=pch.AB),
        text = list(c(A.name, B.name), col=col.AB, cex=.9),
        columns = 2,
        between=.5,
        space="top")
    invisible(list(left.plot, right.plot))
  }
}
                   )

## ae.dotplot(aeanonymr,
##            A.name="TREATMENT A (N=216)",
##            B.name="TREATMENT B (N=431)")