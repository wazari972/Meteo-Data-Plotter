shinyUI(pageWithSidebar(
  headerPanel("Meteo"),

  sidebarPanel(
    wellPanel(
      p(strong("Where")),
      uiOutput("location_selector")
    ),
    wellPanel(
      p(strong("What")),
      checkboxInput(inputId = "what_minimax", label = "Mini-max temperature", value = FALSE),
      checkboxInput(inputId = "what_rain", label = "Rainfall", value = FALSE),
      checkboxInput(inputId = "what_pressure", label = "Pressure", value = TRUE),
      checkboxInput(inputId = "what_humid", label = "Humidity", value = FALSE),
      checkboxInput(inputId = "what_summary", label = "Summary", value = FALSE)
    ),
    wellPanel(
      p(strong("How")),
        checkboxInput(inputId = "average",
                    label = strong("Show average"),
                    value = FALSE),
        checkboxInput(inputId = "regression",
                    label = strong("Show regression curve"),
                    value = TRUE),
        conditionalPanel(condition = "input.regression == true",
                         sliderInput(inputId = "reg_adjust",
                                     label = "Regression adjustment:",
                                     min=0, max=1, value=0.35, step=0.2),
                         checkboxInput(inputId = "opt_daily", label = "Daily values", value = TRUE)
        )
      ),
      conditionalPanel(condition = "input.what_minimax == true",
                       wellPanel(
                         p(strong("Temperature options")),
                         checkboxInput(inputId = "opt_temp_max", label = "Maximal temperature", value = TRUE),
                         checkboxInput(inputId = "opt_temp_med", label = "Average temperature", value = FALSE),
                         checkboxInput(inputId = "opt_temp_min", label = "Minimal temperature", value = FALSE)
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
    conditionalPanel(condition = "input.what_pressure",
        plotOutput(outputId = "plot_pressure")),
    conditionalPanel(condition = "input.what_humid",
        plotOutput(outputId = "plot_humid")),
    conditionalPanel(condition = "input.what_summary",
                     plotOutput(outputId = "plot_summary"))
  )
))
