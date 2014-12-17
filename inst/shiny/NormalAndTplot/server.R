## library(shiny)
## library(HH)

## Define server logic
shinyServer(function(input, output) {

  ## Expression that generates the "NormalAndT-12" plot.
  ## The expression is wrapped in a call to renderPlot to indicate that:
  ##
  ##  1) It is "reactive" and therefore should be automatically
  ##     re-executed when inputs change
  ##  2) Its output type is a plot
  ##

  NormalAndT.interface <- function(
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
    prob.labels,
    cex.main,
    key.axis.padding) {
    NormalAndTplot(
      mean0=mean0,
      mean1=mean1,
      xbar=xbar,
      sd,
      df,
      n,
      c(xlim.lo, xlim.hi),
      c(ylim.lo, ylim.hi),
      alpha.right,
      alpha.left,
      float,
      ntcolors,
      digits=4,
      digits.axis,
      digits.float,
      distribution.name,
      type=HypOrConf,
      z1axis=zaxes,
      zaxis=zaxes,
      cex.z=cex.z,
      cex.prob=cex.prob,
      cex.top.axis,
      ## main, #
      ## xlab, #
      ## ylab, #
      prob.labels=prob.labels,
      cex.main         =cex.main,
      key.axis.padding =key.axis.padding,
      xhalf.multiplier=.65
    )
  }


  ResultNT <- reactive({
    mean0.f <- ifelse(input$HypOrConf=="hypothesis", input$mu0, NA)
    mean1.f <- ifelse(as.logical(input$mu1display), input$mu1, NA)
    xbar.f <- ifelse(as.logical(input$xbardisplay) || input$HypOrConf=="confidence", input$xbar, NA)
    float.f <- as.logical(input$float)
    zaxes.f <- as.logical(input$zaxes)
    prob.labels.f <- as.logical(input$prob.labels)
    distribution.name.f <- ifelse(input$df==0, "z", "t")

    NormalAndT.interface(
      distribution.name=distribution.name.f, ## input$distribution.name,
      mean0            =mean0.f,
      mu1display       =input$mu1display,
      mean1            =mean1.f,
      xbardisplay      =input$xbardisplay,
      xbar             =xbar.f,
      sd               =input$stddev,
      df               =input$df,
      n                =input$n,
      xlim.lo          =input$xlim[1], ##.lo,
      xlim.hi          =input$xlim[2], ##.hi,
      ylim.lo          =0, ## input$ylim.lo,
      ylim.hi          =input$"ylim-hi",
      ## alpha.right      =input$alpha.right,
      ## alpha.left       =input$alpha.left,
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
      prob.labels      =prob.labels.f,
      cex.main         =input$"cex-main",
      key.axis.padding =input$"key-axis-padding"
    )
  })


  Result <- reactive({
    if (input$power %in% c("power","beta") && input$HypOrConf=="hypothesis") ## {
        NormalAndT.and.power(ResultNT(),
                               digits.top.axis=input$"digits-axis",
                               digits.left=input$"digits-float",
                               which=input$power,
                               cex.top.axis=input$"cex-top-axis",
                               cex.left.axis=input$"cex-top-axis")
    else
      ResultNT()
  })

  output$distPlot <- renderPlot({
     print(Result(), tablesOnPlot=as.logical(input$table),
           cex.table=input$"cex-table",
           scales=FALSE, prob=FALSE)
    })

  output$call <- renderText({
    if (as.logical(input$call))
      attr(ResultNT(), "call")
    else ""
  })

})
