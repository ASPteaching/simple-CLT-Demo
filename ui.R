library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel('Sampling Distributions and the Central Limit Theorem'),
  sidebarPanel(
    helpText('Choose your source distribution and number of items, n, in each
             sample. 10000 replications will be run when you click "Sample Now".'),
    
    sliderInput(inputId = "n", "Sample Size n", value = 30, min = 5, max = 100, step = 2),
    
    radioButtons("src.dist", "Distribution type:",
                 c("Exponential: Param1 = Mean" = "E",
                   "Normal:      Param1 = Mean,     Param2 = SD" = "N",
                   "Uniform:     Param1 = Min,      Param2 = Max" = "U",
                   "Poisson:     Param1 = Lambda" = "P",
                   "Cauchy:      Param1 = Location, Param2 = Scale" = "C",
                   "Binomial:    Param1 = Size,     Param2 = Success Probability" = "B",
                   "Gamma:       Param1 = Shape,    Param2 = Scale" = "G",
                   "Chi Square:  Param1 = Degrees of Freedom" = "X",
                   "Student t:   Param1 = Degrees of Freedom" = "T")),
    
    numericInput("param1", "Parameter 1:", value = 10),
    numericInput("param2", "Parameter 2:", value = 2),
    actionButton("takeSample", "Sample Now")
  ),
  
  mainPanel(
    tags$head(
      tags$style("body {background-color: #9999aa; }")
    ),
    plotOutput("plotSample")
  )
)
