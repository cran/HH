library(shiny)
library(HH)

## Define UI for slider demo application
shinyUI(pageWithSidebar(

  ##  Application title
  headerPanel("Population Pyramids"),

  ## Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    ## Simple integer interval
    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput("year", "Year:",
                min=1900, max=1970, value=1900,
                format="####",
                animate=animationOptions(interval=1000, loop=TRUE))
    ),

  ##  Display the generated Population Pyramid
  mainPanel(
    plotOutput("USagePyramidPlot", width="90%", height="600px")
    )
  ))
