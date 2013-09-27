source("print.R")
source("plot.R")
source("config.R")

shinyServer(function (input, output) {
  output$location_selector <- renderUI({
    checkboxGroupInput(inputId = "location",
                       label = "Location:",
                       choices = ls(meteoData))
  })
  
  output$rainthreshold <- renderUI( {
    max <- environment()
    max$Rain <- 1
    
    iterateDataset(selectedDataset(), function(idx, name, data) {
      max$Rain <- max(max$Rain, data$Pluie)
    })
    current <- ifelse(is.null(input$rainthreshold), 0, min(max$Rain, input$rainthreshold))
    
    sliderInput("rainthreshold", "High rainfall threshold (in mm)", min=0, max=max$Rain, value=current)
  })
  
  getVarHeight <- function() {
    return(getFixHeight(length(selectedDataset())))
  }
  
  getFixHeight <- function(nbPlots) {
    HEIGTH <- 400
    return (HEIGTH*nbPlots)
  }
  
  selectedDataset <- reactive({
    return (getDatasets(input$location))
  })
  
  output$plot_rain <- renderPlot({
    splitPlots(selectedDataset(), function(name, data) {
      plot_pluie(name, data$Date, data$Pluie, input)
    })
  }, height=getVarHeight)
  
  output$plot_humid <- renderPlot({
    splitPlots(selectedDataset(), function(name, data) {
      plot_hygro(name, data$Date, data$Hygrometrie, input)
    })
  }, height=getVarHeight)
  
  output$plot_minimax <- renderPlot({
    splitPlots(selectedDataset(), function(name, data) {
      plot_temp(name, data$Date, data$Temp.max, data$Temp.min,  input)
    })
  }, height=getVarHeight)
  
  output$plot_pressure <- renderPlot({
    splitPlots(selectedDataset(), function(name, data) {
      plot_pression(name, data$Date, data$Pression, input)
    })
  }, height=getVarHeight)

  output$plot_computation  <- renderPlot({
      splitPlots(selectedDataset(), function(name, data) {
          plot_computation(name, data$Date, data, input)
      })
  }, height=getVarHeight)
                                       
  output$plot_summary <- renderPlot({
    splitPlots(selectedDataset(), function(name, data) {
      plot_summary(name, data, input) 
    })
  }, height=getFixHeight(4))
  
  
  output$text_rain <- renderPrint({
    iterateDataset(selectedDataset(), function(i, name, data) {
      p(strong(name)) #doesnt work this way
      print_pluie(name, data$Date, data$Pluie, input)
    })
  })
    
  output$text_minimax <- renderPrint({
    iterateDataset(selectedDataset(), function(i, name, data) {
      print_temp(name, data$Date, data$Temp.max, data$Temp.min,  input)
    })
  })
  
  output$text_humid <- renderPrint( {
    iterateDataset(selectedDataset(), function(i, name, data) {
      print_hygro(name, data$Date, data$Hygrometrie, input)
    })
  })
  
  output$text_pressure <- renderPrint({
    iterateDataset(selectedDataset(), function(i, name, data) {
      print_pression(name, data$Date, data$Pression, input)
    })
  })

  output$text_hygro_temp <- renderPrint({
    iterateDataset(selectedDataset(), function(i, name, data) {
#      print_computation(name, data$Date, data$Pression, input)
    })
  })
})
