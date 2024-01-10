# Load required libraries
library(shiny)
library(ggplot2)
library(caret)
library(plotly)

# Load data
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Define UI
ui <- fluidPage(
  titlePanel("Sales Prediction Dashboard"),
  sidebarLayout(
    sidebarPanel(
      numericInput("visitors", "Number of Visitors", value = 200000, min = 150000, max = 260000),
      numericInput("transactions", "Number of Transactions", value = 12000, min = 8000, max = 15000),
      numericInput("items", "Average Items per Transaction", value = 5, min = 4, max = 5.5),
      sliderInput("rating", "Customer Satisfaction Rating", value = 8.9, min = 8, max = 9, step = 0.1),
      numericInput("ads", "Number of Ads", value = 35000, min = 20000, max = 60000)
    ),
    mainPanel(
      plotlyOutput("salesComparisonBarChart"),
      plotOutput("regressionPlot"),
      verbatimTextOutput("regressionSummary")
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive function for regression model
  regression_model <- reactive({
    lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
  })
  
  # Reactive function for predictions
  predictions <- reactive({
    new_data <- data.frame(
      x1 = input$visitors,
      x2 = input$transactions,
      x3 = input$items,
      x4 = input$rating,
      x5 = input$ads
    )
    predict(regression_model(), newdata = new_data)
  })
  
  # Output for interactive bar chart
  output$salesComparisonBarChart <- renderPlotly({
    data_pred <- data.frame(Month = data$Month, Actual_Sales = data$y, Predicted_Sales = predictions())
    
    plot_ly(data_pred, x = ~Month, y = ~Actual_Sales, type = 'bar', name = 'Actual Sales') %>%
      add_trace(y = ~Predicted_Sales, name = 'Predicted Sales') %>%
      layout(title = "Actual vs. Predicted Sales",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Sales"))
  })
  
  # Output for regression plot
  output$regressionPlot <- renderPlot({
    ggplot(data, aes(x = y, y = predictions())) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Sales vs. Predicted Sales",
           x = "Actual Sales",
           y = "Predicted Sales")
  })
  
  # Output for regression summary
  output$regressionSummary <- renderText({
    summary(regression_model())$coefficients
  })
}

# Run the app
shinyApp(ui, server)
