source("plot.R")

data <- meteoGrenoble
size <<- length(data$Date)

shinyServer(function(input, output) {
  output$plot_rain <- reactivePlot(function() { plot_pluie(data$Pluie, input$opt_rain_daily, input$average, input$opt_rain_cumul) })
  output$plot_humid <- reactivePlot(function() { plot_hygro(data$Hygro,  input$average) })
  output$plot_minimax <- reactivePlot(function() { plot_temp(data$Temp.max, data$Temp.min,  
                                                             input$average, input$opt_temp_min, input$opt_temp_max, input$opt_temp_med) })
  output$plot_presure <- reactivePlot(function() { plot_pression(data$Pression,  input$average) })
})