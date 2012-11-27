shinyUI(pageWithSidebar(
  headerPanel("Meteo"),

  sidebarPanel(
    wellPanel(
      p(strong("Options")),
      checkboxInput(inputId = "what_minimax", label = "Mini-max temperature", value = TRUE),
      checkboxInput(inputId = "what_rain", label = "Rainfall", value = TRUE),
      checkboxInput(inputId = "what_presure", label = "Presure", value = TRUE),
      checkboxInput(inputId = "what_humid", label = "Humidity", value = TRUE)
      
    )),

  mainPanel(
    conditionalPanel(condition = "input.what_minimax",
        plotOutput(outputId = "plot_minimax")),
    conditionalPanel(condition = "input.what_rain",
        plotOutput(outputId = "plot_rain")),
    conditionalPanel(condition = "input.what_presure",
        plotOutput(outputId = "plot_presure")),
    conditionalPanel(condition = "input.what_humid",
        plotOutput(outputId = "plot_humid"))
  )
))
