library(shiny)
library(dplyr)

load("pairs_cor.RData")

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Miles Per Gallon"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for
      selectInput("year", "Year:", 2015:2021),
      numericInput("threshold", "Threshold (absolute value):", value = 0),
      numericInput("n_min", "Minimum number of observations:", value = 10)
      
      # Input: Checkbox for whether outliers should be included ----
      # checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      # h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      # plotOutput("mpgPlot")
      DT::dataTableOutput("mytable")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  # formulaText <- reactive({
  #   paste("mpg ~", input$variable)
  # })
  
  # Return the formula text for printing as a caption ----
  # output$caption <- renderText({
  #   formulaText()
  # })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mytable <- DT::renderDataTable({
    pairs |> filter(
      year == input$year, abs(rho) >= input$threshold, n >= input$n_min
    )
  })
  
}

shinyApp(ui, server)