library(shiny)
r <- 10000 # Number of replications... must be ->inf for sampling distribution!

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

server <- function(input, output, session) {
  set.seed(as.numeric(Sys.time()))
  
  rv <- reactiveValues(sample = NULL,
                       all.sums = NULL,
                       all.means = NULL,
                       all.vars = NULL)
  
  observeEvent(input$src.dist, {
    params <- switch(input$src.dist,
                     "E" = list(param1 = list(name = "Mean", value = 1), param2 = NULL),
                     "N" = list(param1 = list(name = "Mean", value = 0), param2 = list(name = "SD", value = 1)),
                     "U" = list(param1 = list(name = "Min", value = 0), param2 = list(name = "Max", value = 1)),
                     "P" = list(param1 = list(name = "Lambda", value = 2), param2 = NULL),
                     "C" = list(param1 = list(name = "Location", value = 0), param2 = list(name = "Scale", value = 1)),
                     "B" = list(param1 = list(name = "Size", value = 10), param2 = list(name = "Success Probability", value = 0.5)),
                     "G" = list(param1 = list(name = "Shape", value = 2), param2 = list(name = "Scale", value = 2)),
                     "X" = list(param1 = list(name = "Degrees of Freedom", value = 2), param2 = NULL),
                     "T" = list(param1 = list(name = "Degrees of Freedom", value = 10), param2 = NULL))
    
    updateNumericInput(session, "param1", label = params$param1$name, value = params$param1$value)
    
    if (!is.null(params$param2)) {
      updateNumericInput(session, "param2", label = params$param2$name, value = params$param2$value)
      shinyjs::show("param2")
    } else {
      shinyjs::hide("param2")
    }
  })
  
  observeEvent(input$takeSample, {
    my.samples <- switch(input$src.dist,
                         "E" = matrix(rexp   (input$n*r, 1 / input$param1), r),
                         "N" = matrix(rnorm  (input$n*r, input$param1, input$param2), r),
                         "U" = matrix(runif  (input$n*r, input$param1, input$param2), r),
                         "P" = matrix(rpois  (input$n*r, input$param1), r),
                         "C" = matrix(rcauchy(input$n*r, input$param1, input$param2), r),
                         "B" = matrix(rbinom (input$n*r, input$param1, input$param2), r),
                         "G" = matrix(rgamma (input$n*r, input$param1, input$param2), r),
                         "X" = matrix(rchisq (input$n*r, input$param1), r),
                         "T" = matrix(rt     (input$n*r, input$param1), r))
    
    rv$sample <- as.numeric(my.samples[1, ])
    rv$all.sums <- as.numeric(apply(my.samples, 1, sum))
    rv$all.means <- as.numeric(apply(my.samples, 1, mean))
    rv$all.vars <- as.numeric(apply(my.samples, 1, var))
  })
  
  output$plotSample <- renderPlot({
    if (input$takeSample) {
      par(mfrow = c(2, 2), oma = c(0, 0, 5, 0))
      hist(rv$sample,    main = "Distribution of One Sample",            ylab = "Frequency", col = 1)
      hist(rv$all.sums,  main = "Sampling Distribution of the Sum",      ylab = "Frequency", col = 2)
      hist(rv$all.means, main = "Sampling Distribution of the Mean",     ylab = "Frequency", col = 3)
      hist(rv$all.vars,  main = "Sampling Distribution of the Variance", ylab = "Frequency", col = 4)
      
      mtext("Simulation Results", outer = TRUE, cex = 3)
    }
  }, height = 660, width = 900)
}
