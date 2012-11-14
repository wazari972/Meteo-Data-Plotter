shinyUI(pageWithSidebar(
  headerPanel("Stocks"),

  sidebarPanel(
    wellPanel(
      p(strong("Stocks")),
      checkboxInput(inputId = "what_minimax", label = "Mini-max temperature", value = TRUE),
      checkboxInput(inputId = "what_rain", label = "Rainfall", value = TRUE),
      checkboxInput(inputId = "what_presure", label = "Presure", value = TRUE),
      checkboxInput(inputId = "what_humid", label = "Humidity", value = TRUE)
      
    ),

    selectInput(inputId = "chart_type",
                label = "Chart type",
                choices = c("Candlestick" = "candlesticks",
                            "Matchstick" = "matchsticks",
                            "Bar" = "bars",
                            "Line" = "line")
    ),

    wellPanel(
      p(strong("Date range (back from present)")),
      sliderInput(inputId = "time_num",
                  label = "Time number",
                  min = 1, max = 24, step = 1, value = 6),

      selectInput(inputId = "time_unit",
                  label = "Time unit",
                  choices = c("Days" = "days",
                              "Weeks" = "weeks",
                              "Months" = "months",
                              "Years" = "years"),
                  selected = "Months")
    ),
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
