shinyUI(pageWithSidebar(
  headerPanel("Meteo"),

  sidebarPanel(
    wellPanel(
      p(strong("Data")),
      checkboxInput(inputId = "what_minimax", label = "Mini-max temperature", value = TRUE),
      checkboxInput(inputId = "what_rain", label = "Rainfall", value = TRUE),
      checkboxInput(inputId = "what_presure", label = "Presure", value = TRUE),
      checkboxInput(inputId = "what_humid", label = "Humidity", value = TRUE)
    ),
    wellPanel(
      p(strong("General options")),
        checkboxInput(inputId = "average",
                    label = strong("Show average"),
                    value = FALSE),
        checkboxInput(inputId = "regression",
                    label = strong("Show regression curve"),
                    value = FALSE),
        conditionalPanel(condition = "input.regression == true",
                         sliderInput(inputId = "reg_adjust",
                                     label = "Regression adjustment:",
                                     min = 0.2, max = 2, value = 1, step = 0.2)
        )
      ),
      conditionalPanel(condition = "input.what_minimax == true",
                       wellPanel(
                         p(strong("Temperature options")),
                         checkboxInput(inputId = "opt_temp_min", label = "Minimal temperature", value = TRUE),
                         checkboxInput(inputId = "opt_temp_med", label = "Average temperature", value = FALSE),
                         checkboxInput(inputId = "opt_temp_max", label = "Maximal temperature", value = TRUE)
                       )
      ),
      conditionalPanel(condition = "input.what_rain == true",
                       wellPanel(
                         p(strong("Rainfall options")),
                         checkboxInput(inputId = "opt_rain_cumul", label = "Cumulative curve", value = TRUE)
                       )
      )
    ),

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
