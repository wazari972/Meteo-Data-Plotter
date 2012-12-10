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
      checkboxInput(inputId = "what_rain", label = "Rainfall", value = TRUE),
      checkboxInput(inputId = "what_pressure", label = "Pressure", value = FALSE),
      checkboxInput(inputId = "what_humid", label = "Humidity", value = FALSE),
      checkboxInput(inputId = "what_summary", label = "Summary", value = FALSE)
    ),
    wellPanel(
      p(strong("How")),
        checkboxInput(inputId = "with_mean",
                    label = strong("Show average"),
                    value = FALSE),
        checkboxInput(inputId = "with_reg",
                    label = strong("Show regression curve"),
                    value = FALSE),
        conditionalPanel(condition = "input.with_reg == true",
                         sliderInput(inputId = "reg_coeff",
                                     label = "Regression adjustment:",
                                     min=0, max=1, value=0.35, step=0.2),
                         checkboxInput(inputId = "with_daily", label = "Daily values", value = TRUE)
        )
      ),
      conditionalPanel(condition = "input.what_minimax == true",
                       wellPanel(
                         p(strong("Temperature options")),
                         checkboxInput(inputId = "with_max", label = "Maximal temperature", value = TRUE),
                         checkboxInput(inputId = "with_med", label = "Average temperature", value = FALSE),
                         checkboxInput(inputId = "with_min", label = "Minimal temperature", value = TRUE),
                         checkboxInput(inputId = "with_zero", label = "With zero", value = TRUE)
                       )
      ),
      conditionalPanel(condition = "input.what_rain == true",
                       wellPanel(
                         p(strong("Rainfall options")),
                         checkboxInput(inputId = "with_cumul", label = "Cumulative curve", value = TRUE),
                         uiOutput("rainthreshold")
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
                     plotOutput(outputId = "plot_summary", height = "400px"))
  )
))
