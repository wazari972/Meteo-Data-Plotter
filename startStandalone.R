source("plot.R")
source("config.R")

input <- frame()
input$with_mean <- TRUE
input$with_daily <- TRUE
input$with_reg <- FALSE
input$reg_coeff <- 0.4

input$with_cumul <- TRUE
input$rainthreshold <- 20

input$with_max <- TRUE
input$with_med <- FALSE
input$with_min <- TRUE
input$with_zero <- TRUE

splitPlots(getDatasets(files), function(name, data) {
  print(name)
  
  #plot_pluie(name, data$Date, data$Pluie, input)
  #plot_hygro(name, data$Date, data$Hygrometrie, input)
  #plot_temp(name, data$Date, data$Temp.max, data$Temp.min,  input)
  #plot_pression(name, data$Date, data$Pression, input)
  plot_summary(name, data, input)
})