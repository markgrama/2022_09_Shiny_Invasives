#### SETUP ####

# Load data
source("CURRENT_DATA.R")
load(paste(npms_label))

library(dplyr)
library(lubridate)
library(tidyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
#

#### UI ####

ui <- fluidPage(
  
  # App title ----
  titlePanel("Regression "),
  
  # Sidebar layout with input and output definitions ----
  fluidRow(
    
    column(
      2, "",
      mainPanel(
        # Input: Selector for
        # selectInput("year", "Year:", 2015:2021),
        selectInput(
          "kew_id", "Kew ID", 
          d_plant |> filter(StaceIV_nativity != "N") |> pull(kew_id) |> unique()
        )
        # uiOutput("inputGroup")
        # numericInput(
        #   "threshold", "Threshold correlation (absolute value):", value = 0
        # ),
        # numericInput("n_min", "Minimum number of observations:", value = 10)
        
        # Input: Checkbox for whether outliers should be included ----
        # checkboxInput("outliers", "Show outliers", TRUE)
        
      )
    ),
    column(
      5, "",
      mainPanel(
        # This is the dynamic UI for the plots
        uiOutput("plots")
        # uiOutput("tables")
        # htmlOutput("tables")
      )
    ),
    column(
      5, "",
      mainPanel(
        # This is the dynamic UI for the plots
        # uiOutput("plots"),
        uiOutput("tables")
        # htmlOutput("tables")
      )
    )
    # Main panel for displaying outputs ----
    # mainPanel(
    #   # Output: Formatted text for caption ----
    #   # tableOutput("table_reg")
    #   # textOutput("table_reg"),
    #   # DT::dataTableOutput("table_reg")
    #   plotOutput("plot_reg"),
    #   htmlOutput("table_reg")
    # )
    # mainPanel(
    #   # This is the dynamic UI for the plots
    #   uiOutput("plots"),
    #   uiOutput("tables")
    #   # htmlOutput("tables")
    # )
  )
)

#### SERVER ####

server <- function(input, output) {
  output$plots <- renderUI({
    plot_output_list <- lapply(2015:2021, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 320, width = 500)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$tables <- renderUI({
    table_output_list <- lapply(2015:2021, function(i) {
      tablename <- paste("table", i, sep="")
      htmlOutput(tablename)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, table_output_list)
  })
  
  
  # Data
  pivot_reac <- function(year, kew_local){
    # Id's for adding up native abundance
    id_native <- d_plant |> 
      filter(StaceIV_nativity == "N", year(date) == year) |>
      pull(kew_id) |> unique()
    
    d_plant |>
      filter(is.na(StaceIV_nativity) == F, year(date) == year) |>
      distinct(kew_id, location_id, .keep_all = T) |>
      select(kew_id, midPoint, location_id) |>
      pivot_wider(
        names_from = kew_id, values_from = midPoint, values_fill = NA
      ) |> mutate(
        ab_native = rowSums(across(.cols = contains(id_native)), na.rm = T)
      ) |> select(ab_native, kew_local) |> as.data.frame()
  }
  
  f_reg <- function(year, kew_local){
    lm(
      data = pivot_reac(year = year, kew_local = kew_local), ab_native ~ .
    )
  }
  
  for (i in 2015:2021) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      
      # Function to actually render a plot
      plotname <- paste("plot", my_i, sep = "")
      output[[plotname]] <- renderPlot({
        # Calculate regression
        reg <- f_reg(year = my_i, kew_local = input$kew_id)
        
        # Bind results
        reg_predict <- cbind(
          pivot_reac(year = my_i, kew_local = input$kew_id) |> 
            na.omit(), predict(reg, interval = 'confidence')
        )
        
        # Produce plot
        ggplot(reg_predict, aes(reg_predict[, paste(input$kew_id)], ab_native)) +
          geom_point() + 
          geom_line(aes(reg_predict[, paste(input$kew_id)], fit)) +
          geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
          xlab(paste(input$kew_id)) + ylab("Native abundance") +
          ggtitle(paste(my_i)) +
          theme(
            title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 18, face = "bold"),
            axis.text.y = element_text(size = 18, face = "bold")
          )
      })
      
      # Tables 
      tablename <- paste("table", my_i, sep = "")
      output[[tablename]] <- reactive({
        html_reg <- f_reg(year = my_i, kew_local = input$kew_id) |>
          sjPlot::tab_model(dv.labels = paste(my_i))
        HTML(html_reg$page.content, html_reg$page.style)
      })
    })
  }
}

shinyApp(ui, server)
#