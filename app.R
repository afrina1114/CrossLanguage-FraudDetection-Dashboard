library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load the results from Python
data <- read.csv("fraud_results.csv")

# UI layout
ui <- fluidPage(
  titlePanel("💳 Fraud Detection Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filter", "Select View:", choices = c("All", "Only Fraud", "Only Legit")),
      helpText("Data from ML model in Python")
    ),
    
    mainPanel(
      DTOutput("table"),
      plotOutput("amountPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$filter == "Only Fraud") {
      data %>% filter(Prediction == 1)
    } else if (input$filter == "Only Legit") {
      data %>% filter(Prediction == 0)
    } else {
      data
    }
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$amountPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Amount, fill = as.factor(Prediction))) +
      geom_histogram(bins = 50, alpha = 0.7) +
      scale_fill_manual(values = c("0" = "steelblue", "1" = "red"), name = "Prediction") +
      labs(title = "Transaction Amount Distribution", x = "Amount ($)", y = "Count")
  })
}

# Run app
shinyApp(ui, server)

