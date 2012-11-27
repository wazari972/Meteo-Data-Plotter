source("plot.R")

data <- meteoGrenoble

shinyServer(function(input, output) {
  output$plot_rain <- reactivePlot(function() { plot_pluie(data$Pluie) })
  output$plot_humid <- reactivePlot(function() { plot_hygro(data$Hygro) })
  output$plot_minimax <- reactivePlot(function() { plot_temp(data$Temp.max, data$Temp.min) })
  output$plot_presure <- reactivePlot(function() { plot_pression(data$Pression) })
})