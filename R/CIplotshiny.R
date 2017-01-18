## library(shiny)

## if (FALSE) {
## shiny.CIplot()
## shiny.CIplot(height=800)  ## px
## }

shiny.CIplot <- function(height="auto") {

  shinyApp(
    ui=shinyUI(fluidPage(

      ## Application title
      titlePanel("Confidence Intervals"),

      ## Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          sliderInput("n.intervals",
                      "Number of Confidence Interval:",
                      min = 10,
                      max = 150,
                      value = 100),
          sliderInput("n.per.row",
                      "Number of Observations for each Estimated Interval:",
                      min = 10,
                      max = 60,
                      value = 40),
          sliderInput("pop.mean",
                      "Population Mean:",
                      min = -10,
                      max = 100,
                      value = 0),
          sliderInput("pop.sd",
                      "Population Standard Deviation:",
                      min = .1,
                      max = 10,
                      value = 1),
          actionButton("NewObs", "New data, same settings",
                       icon = icon("refresh")),
          sliderInput("conf.level",
                      "Confidence Level (same generated data):",
                      min = .50,
                      max = .99,
                      value = .95,
                      step=.01,
                      animate=list())
          ## ,
          ## numericInput("seed", "Random Seed:", NULL),
          ## checkboxInput("keep.same.dataset", "Keep same dataset?",
          ##              value = FALSE, width = NULL),
          ## numericInput("ylim.lo", "ylim low:", NULL),
          ## numericInput("ylim.hi", "ylim high:", NULL),

          ## fluidRow(
          ##   column(4,
          ##          numericInput("xlim.lo", "xlim low:", NULL, width="100px")
          ##          ),
          ##   column(4,
          ##          numericInput("xlim.hi", "xlim high:", NULL, width="100px")
          ##          )
          ## )
        ),

        ## Show a plot of the set of intervals
        mainPanel(
          plotOutput("CIplot")
        )
      )
    )),

    server=function(input, output) {
      ## Expression that generates a CIplot. The expression is
      ## wrapped in a call to renderPlot to indicate that:
      ##
      ##  1) It is "reactive" and therefore should be automatically
      ##     re-executed when inputs change
      ##  2) Its output type is a plot


      tmp.data <-
        reactive({if(input$NewObs+1)
          confintervaldata(n.intervals=input$n.intervals,
                           n.per.row=input$n.per.row,
                           pop.mean=input$pop.mean,
                           pop.sd=input$pop.sd)
        })


      output$CIplot <- renderPlot(
        {
          ## draw the CI plot as specified

          tmp.ci <- confinterval.matrix(tmp.data(),
                                        conf.level=input$conf.level)

          xlim <- attr(tmp.ci, "pop.mean") +
            c(-6, 6) * attr(tmp.ci, "pop.sd") / sqrt(attr(tmp.ci, "n.per.row"))

          ## xlim <- c(input$xlim.lo, input$xlim.hi)
          ## ylim <- c(input$ylim.lo, input$ylim.hi)
          confintervalplot(tmp.ci, xlim=xlim) ##, ylim=ylim)

        },
        height=height)

    }
  )
}
