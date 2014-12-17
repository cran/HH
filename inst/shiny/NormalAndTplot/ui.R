## library(shiny)
## library(HH)

## Define UI for application
shinyUI(fluidPage(

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
      tabPanel("Content",
               column(3,
                      ## radioButtons("distribution.name", NULL,  c("normal","t"), "normal", inline=TRUE),
                      div(class="sliderInputOverride", "mu[0]",
                          sliderInput("mu0", NULL, -10, 10, 0, .1, width="150px")),
                      div(class="sliderInputOverride", "mu[1]",
                          sliderInput("mu1", NULL, -10, 10, 2.5, .1, animate=list(interval=2000), width="150px")),
                      div(class="sliderInputOverride", "xbar",
                          sliderInput("xbar", NULL, -10, 10, 2.1, .1, animate=list(interval=2000), width="150px"))
                      ),
               column(3,
                      div(class="sliderInputOverride", "xlim", sliderInput("xlim", NULL, -15, 15, c(-3, 5), .5, width="150px")),
                      div(class="numericOverride", "ylim-hi",
                          numericInput("ylim-hi",      NULL, .4, min=.01, step=.1)),
                      ## div(class="radioInputOverride", NULL,
                          radioButtons("mu1display", NULL, c("display mu[1]"=TRUE, No=FALSE), TRUE, inline=TRUE), ## ),
                      ## div(class="radioInputOverride", NULL,
                          radioButtons("xbardisplay", NULL, c("display xbar"=TRUE, No=FALSE), FALSE, inline=TRUE) ##)
                      ),
               column(3,
                      div(class="sliderInputOverride", "alpha/conf: left, center, right",
                          sliderInput("alpha", NULL,  0, 1, c(0, .95), .005, width="200px")),
                      div(class="sliderInputOverride", "s/sigma",
                          sliderInput("stddev", NULL, .1, 10, 1, .1, animate=list(interval=2000), width="150px"))
                      ),
               column(3,
                      div(class="sliderInputOverride", "df (0=normal)",
                          sliderInput("df", NULL, 0, 40, 0, 1, animate=list(interval=2000), width="150px")),
                      div(class="sliderInputOverride", "n",
                          sliderInput("n", NULL, 1, 100, 1, 1, animate=list(interval=2000), width="150px"))
                      ## sliderInput("alpha.right", "alpha[right]", 0, .50, .05, .005),
                      ## sliderInput("alpha.left", "alpha[left]",  0, .50, .0, .005)
                      ## numericInput("ylim.lo", "ylim.lo",  0),
                      )),
      tabPanel("Display Options",
               column(3,
                      radioButtons("HypOrConf", NULL, c("Hypothesis"="hypothesis", "Confidence"="confidence"), "hypothesis", inline=TRUE),
                      radioButtons("power", NULL, c(Power="power", Beta="beta", No=FALSE), "power", inline=TRUE)
                      ),
               column(3,
                      radioButtons("float", NULL, c("Prob on Graph"=TRUE, No=FALSE), TRUE, inline=TRUE),
                      radioButtons("prob.labels", NULL, c("Prob Labels"=TRUE, No=FALSE), TRUE, inline=TRUE)
                      ),
               column(3,
                      radioButtons("table", NULL, c("Display Table"=TRUE, "No"=FALSE), TRUE, inline=TRUE),
                      radioButtons("call", NULL, c("Display Call"=TRUE, No=FALSE), FALSE, inline=TRUE)
                      ),
               column(3,
                      radioButtons("ntcolors", NULL, c("Original Colors"="original", Stoplight="stoplight"), "original", inline=TRUE),
                      radioButtons("zaxes", NULL, c("Display Z Axes"=TRUE, No=FALSE), TRUE, inline=TRUE)
                      )
                      ),
      tabPanel("Fonts",
               column(2,
                      div(class="numericOverride", "digits-axis",
                          numericInput("digits-axis",      NULL,  4,   min=1,  step=1)),  br(),
                      div(class="numericOverride", "digits-float",
                          numericInput("digits-float",     NULL,  4,   min=1,  step=1)),  br()
                      ),
               column(2,
                      div(class="numericOverride", "cex-top-axis",
                          numericInput("cex-top-axis",     NULL,  1,   min=.1, step=.1)), br(),
                      div(class="numericOverride", "cex-prob",
                          numericInput("cex-prob",         NULL,  1,   min=.1, step=.1)), br()
                      ),
               column(2,
                      div(class="numericOverride", "cex-z",
                          numericInput("cex-z",            NULL, .7,   min=.1, step=.1)), br(),
                      div(class="numericOverride", "cex-table",
                          numericInput("cex-table",        NULL,  1.2, min=.1, step=.1)), br()
                      ),
               column(3,
                      div(class="numericOverride", "cex-main",
                          numericInput("cex-main",         NULL,  1.6, min=.1, step=.1)), br(),
                      div(class="numericOverride", "key-axis-padding",
                          numericInput("key-axis-padding", NULL,  7,   min=.1, step=.1)), br()
                      )
               )
    )
  )
  )))
