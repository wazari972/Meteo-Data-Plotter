source("config.R")
source("plot.R")

shinyServer(function(input, output) {
  
  selectedDataset <- reactive(function() {
    if (is.null(input$location)) {
      return (list())
    }
    return (list(meteoData[[input$location]]))
  })
  
  output$location_selector <- reactiveUI(function() {
    selectInput(inputId = "location",
                label = "Location:",
                choices = ls(meteoData))
  })
  
  output$plot_rain <- reactivePlot(function() {
    for (data in selectedDataset()) {
      plot_pluie(data$Date, data$Pluie, 
                 input$opt_daily, input$average, 
                 input$opt_rain_cumul,
                 input$regression, input$reg_adjust)
    }
  })
  
  output$plot_humid <- reactivePlot(function() { 
    for (data in selectedDataset()) {
      plot_hygro(data$Date, data$Hygro, 
                 input$opt_daily,  input$average,
                 input$regression, input$reg_adjust)
    } 
  })
  
  output$plot_minimax <- reactivePlot(function() { 
    for (data in selectedDataset()) {
      plot_temp(data$Date, data$Temp.max, data$Temp.min,  
                input$opt_daily, input$average, 
                input$opt_temp_min, input$opt_temp_max, input$opt_temp_med,
                input$regression, input$reg_adjust)
    }
  })
  
  output$plot_pressure <- reactivePlot(function() {
    for (data in selectedDataset()) {
      plot_pression(data$Date, data$Pression,  
                    input$opt_daily, input$average, 
                    input$regression, input$reg_adjust)
    }
  })
})