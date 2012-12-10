source("plot.R")
source("config.R")

shinyServer(function (input, output) {
  output$location_selector <- reactiveUI(function() {
    checkboxGroupInput(inputId = "location",
                       label = "Location:",
                       choices = ls(meteoData))
  })
  
  output$rainthreshold <- reactiveUI(function() {
    max <- environment()
    max$Rain <- 1
    
    iterateDataset(selectedDataset(), function(idx, name, data) {
      max$Rain <- max(max$Rain, data$Pluie)
    })
    current <- ifelse(is.null(input$rainthreshold), 0, min(max$Rain, input$rainthreshold))
    
    sliderInput("rainthreshold", "High rainfall threshold (in mm)", min=0, max=max$Rain, value=current)
  })
  
  selectedDataset <- reactive(function () {
    ret = list(NULL)
    
    if (length(input$location) == 0)
      return (ret)
    
    for(i in 1:length(input$location)) {
      ret[[i]] <- list(input$location[i], meteoData[[input$location[i]]])
    }
    
    return (ret)
  })  
  
  output$plot_rain <- reactivePlot(function() {
    splitPlots(selectedDataset(), function(name, data) {
      plot_pluie(name, data$Date, data$Pluie, input)
    })
  })
  
  output$plot_humid <- reactivePlot(function() { 
    splitPlots(selectedDataset(), function(name, data) {
      plot_hygro(name, data$Date, data$Hygrometrie, input)
    })
  })
  
  output$plot_minimax <- reactivePlot(function() { 
    splitPlots(selectedDataset(), function(name, data) {
      plot_temp(name, data$Date, data$Temp.max, data$Temp.min,  input)
    })
  })
  
  output$plot_pressure <- reactivePlot(function() {
    splitPlots(selectedDataset(), function(name, data) {
      plot_pression(name, data$Date, data$Pression, input)
    })
  })
                                       
  output$plot_summary <- reactivePlot(function() { 
    splitPlots(selectedDataset(), function(name, data) {
      plot_summary(name, data, input) 
    })
  })
})