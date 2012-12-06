source("plot.R")

meteoGrenoble <- read.csv(file="Grenoble.csv",head=TRUE,sep=";")
meteoMartinique <- read.csv(file="Martinique.csv",head=TRUE,sep=";")

meteoGrenoble <- fix_data(meteoGrenoble)
meteoMartinique <- fix_data(meteoMartinique)

data <- meteoGrenoble
size <- length(data$Date)

shinyServer(function(input, output) {
  data <- meteoGrenoble
  size <- length(data$Date)
  
  output$plot_rain <- reactivePlot(function() {
    par(mfrow = c(2, 1))
    plot_pluie(data$Pluie, 
               input$opt_daily, input$average, 
               input$opt_rain_cumul,
               input$regression, input$reg_adjust)
    
    #Store the x-axis data of the top plot so it can be used on the other graphs
    #par(new=TRUE)
    pardat<-par()
    xaxisdat<-seq(pardat$xaxp[1],pardat$xaxp[2],(pardat$xaxp[2]-pardat$xaxp[1])/pardat$xaxp[3])
    yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
    axis(2, at=yaxisdat)
    
    data <- meteoMartinique
    size <- length(data$Date)
    
    plot_pluie(data$Pluie, 
               input$opt_daily, input$average, 
               input$opt_rain_cumul,
               input$regression, input$reg_adjust)
    
    })

  output$plot_humid <- reactivePlot(function() { plot_hygro(data$Hygrometrie, 
                                                            input$opt_daily,  input$average,
                                                            input$regression, input$reg_adjust) })
  
  output$plot_minimax <- reactivePlot(function() { plot_temp(data$Temp.max, data$Temp.min,  
                                                             input$opt_daily, input$average, 
                                                             input$opt_temp_min, input$opt_temp_max, input$opt_temp_med,
                                                             input$regression, input$reg_adjust) })
  
  output$plot_presure <- reactivePlot(function() { plot_pression(data$Pression,  
                                                                 input$opt_daily, input$average, 
                                                                 input$regression, input$reg_adjust) })
  output$plot_summary <- reactivePlot(function() { plot_summary(data) })
})