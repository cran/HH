shiny.NormalAndTplot <- function(x=NULL, ...) {
  UseMethod("shiny.NormalAndTplot")
}

shiny.NormalAndTplot.htest <- function(x=NULL, ..., NTmethod="htest") {
  shiny.NormalAndTplot(NormalAndTplot.htest(x, ...), ..., NTmethod=NTmethod)
}

shiny.NormalAndTplot.default <- function(x=NULL, ...) {
  ## ignore incoming x
  distribution.name <- list(...)$distribution.name
  if (is.null(distribution.name) ||
      (!is.null(distribution.name) && distribution.name != "binomial"))
    shiny.NormalAndTplot(NormalAndTplot.default(...))
  else {
    xlab <- list(...)$xlab
    if (is.null(xlab))
      xlab <- '"w = p = population proportion"'
    shiny.NormalAndTplot(NormalAndTplot.default(..., xlab=xlab), df=1)
  }
}



shiny.NormalAndTplot.NormalAndTplot <- function(x=NULL, ..., NT=attr(x, "call.list"), NTmethod="default") {

  if (FALSE) {
    ## this is the content and structure of the
    ## call.list from NormalAndTplot.default
    list(mean0=ifelse(type=="hypothesis", mean0, NA),
         mean1=mean1,
         xbar=ifelse(type=="confidence", mean0, xbar),
         sd=sd,
         df=df,
         n=n,
         xlim=xlim,
         ylim=ylim,
         alpha.right=alpha.right,
         alpha.left=alpha.left,
         float=float,
         ntcolors=ntcolors,
         digits=digits,
         distribution.name=distribution.name,
         type=type,
         zaxis=zaxis,
         cex.z=cex.z,
         cex.prob=cex.prob,
         main=main,
         xlab=xlab,
         ## ylab=ylab,
         prob.labels=prob.labels,
         ## xhalf.multiplier=xhalf.multiplier,
         cex.main=1,
         key.axis.padding=4.5,
         number.vars=1,
         sub=NULL,
         NTmethod=NTmethod,
         power=power,
         beta=beta,
         ...
         )
  }

  mean0=NT$mean0
  mean1=NT$mean1
  xbar=NT$xbar
  sd=NT$sd
  logsd=log(sd, 10)
  df=NT$df
  n=NT$n
  stderr=sd/sqrt(n)
  logstderr=log(stderr, 10)
  xlim.initial=NT$xlim
  xlim.potential=NT$xlim + c(-1.1, 1.1)*stderr
  xlim.xbar=NT$xlim + c(-1, 1)*stderr
  diff.xlim=diff(NT$xlim)/100
  ylim=NT$ylim
  alpha.right=NT$alpha.right
  alpha.left=NT$alpha.left
  float=NT$float
  ntcolors=NT$ntcolors
  digits=NT$digits
  distribution.name=NT$distribution.name
  type=NT$type
  zaxis=NT$zaxis
  cex.z=NT$cex.z
  cex.prob=NT$cex.prob
  main=NT$main
  xlab=NT$xlab
  ## ylab=NT$ylab
  prob.labels=NT$prob.labels
  ## xhalf.multiplier=NT$xhalf.multiplier
  cex.main=NT$cex.main
  key.axis.padding=NT$key.axis.padding
  number.vars=NT$number.vars
  sub=NT$sub
  power=NT$power
  beta=NT$beta
  x.xx=c("xbar","xbar1-xbar2")[number.vars]

  list.dots <- list(...)
  for (i in names(list.dots)) assign(i, list.dots[[i]])

  if (type == "confidence" && is.na(mean0)) mean0 <- xbar
  if (distribution.name %in% c("normal", "z", "binomial") && is.infinite(df)) df <- 0

  mu1display <- (!(is.null(mean1)||is.na(mean1)))
  xbardisplay <- (!(is.null(xbar)||is.na(xbar)))
  mean1 <- if (mu1display) mean1 else mean0+2*sd  ## arbitrary value
  xbar <- if (xbardisplay) xbar else mean0+1.8*sd ## arbitrary value

  ExpressionOrText <- function(x) {
    if (is.character(x)) return(x)
    xdp <-
      if (length(x)>1)
        deparse(x[[1]], width.cutoff=500)
      else
        deparse(x, width.cutoff=500)
    xdp
  }

  shinyApp(
    ui =
fluidPage(

  ## Application title
  titlePanel(title=NULL, windowTitle="NormalAndT-12"),

  ## output
  plotOutput("distPlot", width="100%", height="800px"),
  textOutput("call"),

  ## empty space?
  ## hr(),

  ##2
  tags$head(tags$style(type="text/css",
                       ".sliderInputOverride {display: inline-block; font-size: 12px; line-height: 12px}",
                       ".jslider {display: inline-block; margin-top: 12px}")), ## good
##  tags$head(tags$style(type="text/css", ".sliderAnimatContainerOverride {display: inline-block}")),  ## not clear whether this does anything
##  tags$head(tags$style(type="text/css", ".icon-play {display: inline-block}")),
  tags$head(tags$style(type="text/css",
                       ".radio.inline {font-size: 11px; line-height: 10px}")),
  tags$head(tags$style(type="text/css",
                       ".checkbox.inline {font-size: 11px; line-height: 10px}")),

  tags$head(tags$style(type="text/css", ".numericOverride {display: inline-block}",
                       "input[type=number]::-webkit-inner-spin-button, input[type=number]::-webkit-outer-spin-button { -webkit-appearance: none;  margin: 0;}")),

  tags$head(tags$style(type="text/css", "#ylim-hi          {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#digits-axis      {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#digits-float     {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#cex-table        {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#cex-z            {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#cex-prob         {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#cex-top-axis     {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#cex-main         {width: 20px; height: 15px}")),
  tags$head(tags$style(type="text/css", "#key-axis-padding {width: 20px; height: 15px}")),

  h6(
  ## fluidRow with a slider input and other inputs
  fluidRow(
    tabsetPanel(
      tabPanel("General",
               column(6,
                      radioButtons("Binomial", NULL, c("Normal and t"="NorT", "Normal Approximation to the Binomial"="Binom"), if (distribution.name == "binomial") "Binom" else "NorT", inline=TRUE),
                      radioButtons("HypOrConf", NULL, c("Hypothesis"="hypothesis", "Confidence"="confidence"), type, inline=TRUE),
                      radioButtons("NDF", NULL, c("Ignore df slider"="idfs", "Ignore n slider"="ins", "Honor both df and n sliders"="hon2"),
                                   switch(NTmethod,
                                          default="hon2",
                                          htest="ins",
                                          power.htest="idfs",
                                          binomial="idfs"), inline=TRUE)
                      ),
               column(3,
                      div(class="numericOverride", "ylim-hi",
                          numericInput("ylim-hi",      NULL, if (distribution.name == "binomial") 5 else ylim[2], min=.01, step=.1)),
                      checkboxGroupInput("mu1xbar", NULL, c("Display mu[1]", "Display xbar"),
                                         c("Display mu[1]","Display xbar")[c(mu1display, xbardisplay)],
                                         inline = TRUE)
                      ## ## div(class="radioInputOverride", NULL,
                      ## radioButtons("mu1display", NULL, c("display mu[1]"=TRUE, No=FALSE), mu1display, inline=TRUE), ## ),
                      ## ## div(class="radioInputOverride", NULL,
                      ## radioButtons("xbardisplay", NULL, c("display xbar"=TRUE, No=FALSE), xbardisplay, inline=TRUE) ##)
                      ),
               column(3,
                      div(class="sliderInputOverride", "alpha/conf: left, center, right",
                          sliderInput("alpha", NULL,  0, 1, c(alpha.left, 1-alpha.right), .005, width="200px")),
                      div(class="sliderInputOverride", "n",
                          sliderInput("n", NULL, 1,
                                      100, ## if (NTmethod=="htest") 1.0001 else 100,
                                      n,
                                      1, ## if (NTmethod=="htest") .00005 else 1,
                                      animate=list(interval=2000), width="150px"))
                      )
      ),
      tabPanel("Normal and t",
               column(4,
                      ## radioButtons("distribution.name", NULL,  c("normal","t"), "normal", inline=TRUE),
                      div(class="sliderInputOverride", "mu[0]",
                          sliderInput("mu0", NULL, mean0-50*diff.xlim, mean0+50*diff.xlim, mean0, diff.xlim, width="150px")),
                      div(class="sliderInputOverride", "mu[a]",
                          sliderInput("mu1", NULL, mean1-50*diff.xlim, mean1+50*diff.xlim, mean1, diff.xlim, animate=list(interval=2000), width="150px")),
                      div(class="sliderInputOverride", paste("w=",x.xx, sep=""),
                          sliderInput("xbar", NULL, xbar-50*diff.xlim, xbar+50*diff.xlim, xbar, diff.xlim, animate=list(interval=2000), width="150px"))
                     ),
               column(3,
                      div(class="sliderInputOverride", "xlim", sliderInput("xlim", NULL, xlim.potential[1], xlim.potential[2], xlim.initial, 5*diff.xlim, width="150px"))
                      ),
               column(4,
                      ## div(class="sliderInputOverride", "s/sigma",
                      ##     sliderInput("stddev", NULL, .1, 10, stderr, .1, animate=list(interval=2000), width="150px"))
                      ## div(class="sliderInputOverride", "log(se[xbar], 10)", ##: log(sd/3)--log(sd*3)",
                      ##     sliderInput("logstderr", NULL, -.5+logstderr, .5+logstderr, 0+logstderr, .1, animate=list(interval=2000), width="150px")), ## br(),
                      ## paste(c("se[xbar]: lo","init","hi"), signif(10^(c(-.5+logstderr, logstderr, .5+logstderr)), digits=3), sep="=", collapse=" "), br(), br(),
                      div(class="sliderInputOverride", "log(sd, 10)", ##: log(sd/3)--log(sd*3)",
                          sliderInput("logsd", NULL, -.5+logsd, .5+logsd, 0+logsd, .1, animate=list(interval=2000), width="150px")), ## br(),
                      paste(c("sd: lo","init","hi"), signif(10^(c(-.5+logsd, logsd, .5+logsd)), digits=3), sep="=", collapse=" "), br(), br(),
                      div(class="sliderInputOverride", "df (0=normal)",
                          sliderInput("df", NULL, 0, 200, df, 1, animate=list(interval=2000), width="150px"))
                      )
               ),
      tabPanel("Normal approximation to the Binomial",
               column(4,
                      div(class="sliderInputOverride", "p[0]",
                          sliderInput("p0", NULL, 0, 1, .5, .01, width="150px")),
                      div(class="sliderInputOverride", "p[1]",
                          sliderInput("p1", NULL, 0, 1, .8, .01, animate=list(interval=2000), width="150px")),
                      div(class="sliderInputOverride", "p.hat",
                          sliderInput("p-hat", NULL, 0, 1, .75, .01, animate=list(interval=2000), width="150px"))
                     ),
               column(4,
                      div(class="sliderInputOverride", "xlimBinomial",
                          sliderInput("xlimBinomial", NULL, 0, 1, c(0,1), .1, width="150px"))
                      )
               ),
      tabPanel("Display Options",
               column(5,
##                      radioButtons("power", NULL, c(Power="power", Beta="beta", No=FALSE), "power", inline=TRUE)
                      checkboxGroupInput("displays", NULL, c("Power", "Beta", "Table", "Call", "z axes"), c("Power","Table"), inline=TRUE)
                      ),
               column(4,
                      checkboxGroupInput("probs", NULL, c("Prob values on Graph"="Values","Labels"), c("Values","Labels"), inline=TRUE)
                      ## radioButtons("float", NULL, c("Prob on Graph"=TRUE, No=FALSE), float, inline=TRUE),
                      ## radioButtons("prob.labels", NULL, c("Prob Labels"=TRUE, No=FALSE), TRUE, inline=TRUE)
                      ),
               ## column(3,
               ##        radioButtons("table", NULL, c("Display Table"=TRUE, "No"=FALSE), TRUE, inline=TRUE),
               ##        radioButtons("call", NULL, c("Display Call"=TRUE, No=FALSE), FALSE, inline=TRUE)
               ##        ),
               column(3,
                      radioButtons("ntcolors", NULL, c("Original Colors"="original", Stoplight="stoplight"), ntcolors, inline=TRUE)## ,
                      ## radioButtons("zaxes", NULL, c("Display Z Axes"=TRUE, No=FALSE), zaxis, inline=TRUE)
                      )),
      tabPanel("Fonts",
               column(2,
                      div(class="numericOverride", "digits-axis",
                          numericInput("digits-axis",      NULL,  digits,   min=1,  step=1)),  br(),
                      div(class="numericOverride", "digits-float",
                          numericInput("digits-float",     NULL,  digits,   min=1,  step=1)),  br()
                      ),
               column(2,
                      div(class="numericOverride", "cex-top-axis",
                          numericInput("cex-top-axis",     NULL,  1,   min=.1, step=.1)), br(),
                      div(class="numericOverride", "cex-prob",
                          numericInput("cex-prob",         NULL,  cex.prob,   min=.1, step=.1)), br()
                      ),
               column(2,
                      div(class="numericOverride", "cex-z",
                          numericInput("cex-z",            NULL, cex.z,   min=.1, step=.1)), br(),
                      div(class="numericOverride", "cex-table",
                          numericInput("cex-table",        NULL,  1.2, min=.1, step=.1)), br()
                      ),
               column(3,
                      div(class="numericOverride", "cex-main",
                          numericInput("cex-main",         NULL,  1.6, min=.1, step=.1)), br(),
                      div(class="numericOverride", "key-axis-padding",
                          numericInput("key-axis-padding", NULL,  7,   min=.1, step=.1)), br()
                      ))##,
      ##tabPanel("Constants",
               ##column(6,
                      ##textInput("sub", "Subtitle for graph", sub)
                      ##),
               ##column(6,
                      ##numericInput("number.vars", "number.vars", number.vars)
                      ##)##,
               ##column(6,
               ##       textInput("main", "Main title for graph", ExpressionOrText(main)), br(),
               ##       textInput("xlab", "X label", ExpressionOrText(xlab))
               ##       )
               ##)
    )
  )))
,


###---------

    server =
## library(shiny)
## library(HH)

## Define server logic
function(input, output) {

  ## Expression that generates the "NormalAndT-12" plot.
  ## The expression is wrapped in a call to renderPlot to indicate that:
  ##
  ##  1) It is "reactive" and therefore should be automatically
  ##     re-executed when inputs change
  ##  2) Its output type is a plot
  ##

  NormalAndTInterface <- function(
    distribution.name,
    mean0,
    mu1display,
    mean1,
    xbardisplay,
    xbar,
    sd,
    df,
    n,
    xlim.lo,
    xlim.hi,
    ylim.lo,
    ylim.hi,
    alpha.right,
    alpha.left,
    float,
    ntcolors,
    digits=4,
    digits.axis,
    digits.float,
    HypOrConf,
    zaxes,
    cex.z,
    cex.prob,
    cex.top.axis,
    main,
    xlab,
    prob.labels,
    cex.main,
    key.axis.padding,
    number.vars,
    sub,
    power=power,
    beta=beta) {
##    if (substring(main, 1, 10) == "expression") main <- get("main", envir=sys.parent())
##    if (substring(xlab, 1, 10) == "expression") xlab <- get("xlab", envir=sys.parent())
    NormalAndTplot(
      mean0=mean0,
      mean1=mean1,
      xbar=xbar,
      sd=sd,
      df=df,
      n=n,
      xlim=c(xlim.lo, xlim.hi),
      ylim=c(ylim.lo, ylim.hi),
      alpha.right=alpha.right,
      alpha.left=alpha.left,
      float=float,
      ntcolors=ntcolors,
      digits=4,
      digits.axis=digits.axis,
      digits.float=digits.float,
      distribution.name=distribution.name,
      type=HypOrConf,
      z1axis=zaxes,
      zaxis=zaxes,
      cex.z=cex.z,
      cex.prob=cex.prob,
      cex.top.axis=cex.top.axis,
      main=main, #
      xlab=xlab, #
      ## ylab, #
      prob.labels=prob.labels,
      cex.main=cex.main,
      key.axis.padding=key.axis.padding,
      xhalf.multiplier=.65,
      number.vars=number.vars,
      sub=sub,
      power=power,
      beta=beta
    )
  }


  ResultNT <- reactive({
    NDF <- input$NDF ## idfs, ins, hon2
    mean0.f <- if (input$HypOrConf=="hypothesis") input$mu0 else NA
    mean1.f <- if ("Display mu[1]"  %in% input$mu1xbar) input$mu1 else NA
    xbar.f <-  if ("Display xbar" %in% input$mu1xbar || input$HypOrConf=="confidence") input$xbar else NA
    n.f <- switch(NDF,
                  idfs=input$n,
                  ins=NT$n,
                  hon2=input$n)
    df.f <- switch(NDF,
                   idfs=number.vars*(input$n-1),
                   ins=input$df,
                   hon2=input$df)
    stderr.f <- (10^input$logsd)/sqrt(n.f)
    float.f <- "Values" %in% input$probs ##as.logical(input$float)
    zaxes.f <- "z axes" %in% input$displays ##as.logical(input$zaxes)
    prob.labels.f <- "Labels" %in% input$probs ##as.logical(input$prob.labels)
    distribution.name.f <- if (df.f==0) "z" else "t"
    xlim.lo.f <- input$xlim[1]
    xlim.hi.f <- input$xlim[2]
    xlab.f <- input$xlab
    main.f <- main


    ## Normal Approximation to the Binomial
    if (input$Binomial=="Binom") {
      p0 <- input$p0
      p1 <- input$p1
      p.hat <- input$"p-hat"
      n.f <- input$n
      df.f <- Inf
      sigma.p0 <- sqrt(p0*(1-p0)/n)
      sigma.p1 <- sqrt(p1*(1-p1)/n)
      s.p.hat <- sqrt(p.hat*(1-p.hat)/n)
      z.calc <- (p.hat-p0)/sigma.p0
      mean0.f <- if (input$HypOrConf=="hypothesis") p0 else NA
      mean1.f <- if ("Display mu[1]"  %in% input$mu1xbar) p1 else NA
      xbar.f <- if ("Display xbar" %in% input$mu1xbar || input$HypOrConf=="confidence") p.hat else NA
      stderr.f <- if (type=="hypothesis") sigma.p0 else s.p.hat
      distribution.name.f <- "binomial"
      xlim.lo.f <- input$xlimBinomial[1]
      xlim.hi.f <- input$xlimBinomial[2]
      xlab.f <- "w = p = population proportion"
    }

    NormalAndTInterface(
      distribution.name=distribution.name.f, ## input$distribution.name,
      mean0            =mean0.f,
      mu1display       ="Display mu[1]"  %in% input$mu1xbar,
      mean1            =mean1.f,
      xbardisplay      ="Display xbar" %in% input$mu1xbar,
      xbar             =xbar.f,
      sd               =stderr.f*sqrt(n.f),
      df               =df.f,
      n                =n.f,
      xlim.lo          =xlim.lo.f,
      xlim.hi          =xlim.hi.f,
      ylim.lo          =0, ## input$ylim.lo,
      ylim.hi          =input$"ylim-hi",
      alpha.right      =1-input$alpha[2],
      alpha.left       =input$alpha[1],
      float            =float.f,
      ntcolors         =input$ntcolors,
      digits           =4,
      digits.axis      =input$"digits-axis",
      digits.float     =input$"digits-float",
      HypOrConf        =input$HypOrConf,
      zaxes            =zaxes.f,
      cex.z            =input$"cex-z",
      cex.prob         =input$"cex-prob",
      cex.top.axis     =input$"cex-top-axis",
      main             =main.f,
      xlab             =xlab.f,
      prob.labels      =prob.labels.f,
      cex.main         =input$"cex-main",
      key.axis.padding =input$"key-axis-padding",
      number.vars      =number.vars, ## input$number.vars,
      sub              =sub, ## input$sub,
      power            ="Power" %in% input$displays,
      beta             ="Beta" %in% input$displays
    )
  })


  Result <- reactive({
      ResultNT()
  })

  output$distPlot <- renderPlot({
     print(Result(), tablesOnPlot="Table" %in% input$displays, ## as.logical(input$table),
           cex.table=input$"cex-table",
           scales=FALSE, prob=FALSE)
    })

  output$call <- renderText({
    if ("Call" %in% input$displays) ##(as.logical(input$call))
      attr(ResultNT(), "call")
    else ""
  })

})
}
