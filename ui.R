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
        checkboxInput(inputId = "with_graph", label = "Show graphs", value = TRUE),
        checkboxInput(inputId = "with_text", label = "Show facts", value = TRUE),
        checkboxInput(inputId = "with_mean",
                    label = "Show average",
                    value = FALSE),
        checkboxInput(inputId = "with_smooth",
                    label = "Show smooth curve",
                    value = FALSE),
        conditionalPanel(condition = "input.with_smooth == true",
                         sliderInput(inputId = "smooth_coeff",
                                     label = "Smoothness adjustment:",
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
        conditionalPanel(condition = "input.with_graph",
          plotOutput(outputId = "plot_minimax", height="100%")),
        conditionalPanel(condition = "input.with_text", p(strong("Mini-max temperature")),
                     verbatimTextOutput(outputId = "text_minimax"))),

    conditionalPanel(condition = "input.what_rain",
        conditionalPanel(condition = "input.with_graph",
          plotOutput(outputId = "plot_rain", height="100%")),
        conditionalPanel(condition = "input.with_text", p(strong("Rainfall")),
                      verbatimTextOutput(outputId = "text_rain"))),
    
    conditionalPanel(condition = "input.what_pressure",
        conditionalPanel(condition = "input.with_graph",
          plotOutput(outputId = "plot_pressure", height="100%")),
        conditionalPanel(condition = "input.with_text", p(strong("Pressure")),
                    verbatimTextOutput(outputId = "text_pressure"))),
    
    conditionalPanel(condition = "input.what_humid",
        conditionalPanel(condition = "input.with_graph",
          plotOutput(outputId = "plot_humid", height="100%")),
        conditionalPanel(condition = "input.with_text", p(strong("Humidity")), 
                         verbatimTextOutput(outputId = "text_humid"))),
    
    conditionalPanel(condition = "input.what_summary",
                     plotOutput(outputId = "plot_summary", height="100%"))
  )
))
