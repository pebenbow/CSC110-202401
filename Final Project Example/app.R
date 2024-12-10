library(shiny)
library(ggplot2)
library(dplyr)

# Load the airquality dataset
data("airquality")

# Define UI
ui <- fluidPage(
  titlePanel("Air Quality Dashboard"),
  tabsetPanel(
    tabPanel(
      "Visualizations",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            "monthFilter",
            "Select Month:",
            min = 5, max = 9,
            value = c(5, 9),
            step = 1
          ),
          sliderInput(
            "tempFilter",
            "Temperature Range:",
            min = min(airquality$Temp, na.rm = TRUE),
            max = max(airquality$Temp, na.rm = TRUE),
            value = range(airquality$Temp, na.rm = TRUE)
          )
        ),
        mainPanel(
          plotOutput("tempPlot"),
          plotOutput("ozonePlot")
        )
      )
    ),
    tabPanel(
      "User Guide",
      includeMarkdown("userGuide.md")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    airquality %>%
      filter(
        Month >= input$monthFilter[1],
        Month <= input$monthFilter[2],
        Temp >= input$tempFilter[1],
        Temp <= input$tempFilter[2]
      )
  })
  
  output$tempPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Temp, y = Ozone)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        title = "Temperature vs. Ozone Levels",
        x = "Temperature",
        y = "Ozone"
      ) +
      theme_minimal()
  })
  
  output$ozonePlot <- renderPlot({
    ggplot(filteredData(), aes(x = factor(Month), y = Ozone)) +
      geom_boxplot() +
      labs(
        title = "Ozone Levels by Month",
        x = "Month",
        y = "Ozone"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
