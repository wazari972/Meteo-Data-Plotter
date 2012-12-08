source("plot.R")
source("config.R")

splitPlot <- function (idx, len) {
  if (idx == 1) {
    par(mfrow=c(len, 1))
  } else {
    #par()
  }
}

shinyServer(function(input, output) {
  selectedDataset <- reactive(function() {
    ret = list(NULL)
    
    if (length(input$location) == 0)
      return (ret)
    
    for(i in 1:length(input$location)) {
      ret[i] <- list(meteoData[[input$location[i]]])
    }
    
    return (ret)
  })
  
  output$location_selector <- reactiveUI(function() {
    checkboxGroupInput(inputId = "location",
                label = "Location:",
                choices = ls(meteoData))
  })
  
  output$plot_rain <- reactivePlot(function() {
    dataset = selectedDataset()
    
    if (length(dataset) == 0 || is.null(dataset[[1]])) {
      return ()
    } 
    
    for (i in 1:length(dataset)) {
      data = dataset[[i]]
      
      splitPlot(i, length(dataset))
      
      plot_pluie(data$Date, data$Pluie, input)
    }
  })
  
  output$plot_humid <- reactivePlot(function() { 
    dataset = selectedDataset()
    
    if (length(dataset) == 0 || is.null(dataset[[1]])) {
      return ()
    } 
    
    for (i in 1:length(dataset)) {
      data = dataset[[i]]
      
      splitPlot(i, length(dataset))
      
      plot_hygro(data$Date, data$Hygrometrie, input)
    } 
  })
  
  output$plot_minimax <- reactivePlot(function() { 
    dataset = selectedDataset()
    
    if (length(dataset) == 0 || is.null(dataset[[1]])) {
      return ()
    } 
    
    for (i in 1:length(dataset)) {
      data = dataset[[i]]
      
      splitPlot(i, length(dataset))
      
      plot_temp(data$Date, data$Temp.max, data$Temp.min,  input)
    }
  })
  
  output$plot_pressure <- reactivePlot(function() {
    dataset = selectedDataset()
    
    if (length(dataset) == 0 || is.null(dataset[[1]])) {
      return ()
    } 
    
    for (i in 1:length(dataset)) {
      data = dataset[[i]]
      
      splitPlot(i, length(dataset))
      
      plot_pression(data$Date, data$Pression, input)
    }
  output$plot_summary <- reactivePlot(function() { plot_summary(data) })
  })
})