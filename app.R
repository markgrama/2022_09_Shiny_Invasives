#### SETUP ####

# Load data
source("CURRENT_DATA.R")
load(paste(npms_label))

library(dplyr)
library(lubridate)
library(tidyr)
library(sjPlot)
library(ggplot2)
#

#### UI ####

ui <- fluidPage(
  
  # App title ----
  titlePanel("Regression "),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for
      selectInput("year", "Year:", 2015:2021),
      uiOutput("inputGroup")
      # numericInput(
      #   "threshold", "Threshold correlation (absolute value):", value = 0
      # ),
      # numericInput("n_min", "Minimum number of observations:", value = 10)
      
      # Input: Checkbox for whether outliers should be included ----
      # checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      # h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      # tableOutput("table_reg")
      # textOutput("table_reg"),
      # DT::dataTableOutput("table_reg")
      plotOutput("plot_reg"),
      htmlOutput("table_reg")
    )
  )
)

#### SERVER ####

server <- function(input, output) {
  # Create the correct list of alien for UI
  observeEvent(
    input$year, {
      output$inputGroup <- renderUI({
        selectInput(
          "kew_id", "Kew id:",
          d_plant |>
            filter(StaceIV_nativity != "N", year(date) == input$year) |>
            pull(kew_id) |> unique()
        )
      })
    }
  )
  
  # Native ID's
  id_native <- reactive({
    d_plant |> 
      filter(StaceIV_nativity == "N", year(date) == input$year) |>
      pull(kew_id) |> unique()
  })
  
  # Data
  pivot_reac <- reactive({
    id_local <- id_native()
    d_plant |>
      filter(is.na(StaceIV_nativity) == F, year(date) == input$year) |>
      distinct(kew_id, location_id, .keep_all = T) |>
      select(kew_id, midPoint, location_id) |>
      pivot_wider(
        names_from = kew_id, values_from = midPoint, values_fill = NA
      ) |> mutate(
        ab_native = rowSums(across(.cols = contains(id_local)), na.rm = T)
      ) |> select(ab_native, input$kew_id) |> as.data.frame()
  })
  
  # Compute regression
  reg <- reactive({lm(data = pivot_reac(), ab_native ~ 0 + .)})
  ## Dataset for plot
  reg_predict <- reactive({
    cbind(
      pivot_reac() |> na.omit(), predict(reg(), interval = 'confidence')
    )
  })
  
  # Plot the regression
  output$plot_reg <- renderPlot({
    reg_local <- reg_predict()
    ggplot(reg_local, aes(reg_local[, paste(input$kew_id)], ab_native)) +
      geom_point() + 
      geom_line(aes(reg_local[, paste(input$kew_id)], fit)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
      xlab(paste(input$kew_id)) + ylab("Native abundance") +
      ggtitle(paste(input$kew_id)) +
      theme(
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold")
      )
  })
  
  # Table regression
  output$table_reg <- reactive({
    html_reg <- reg() |> sjPlot::tab_model()
    HTML(html_reg$page.content, html_reg$page.style)
  })
}

shinyApp(ui, server)
#