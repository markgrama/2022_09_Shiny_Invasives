#### SETUP ####

# setwd("C:/Users/mgram/Repos/2022_09_Shiny_Invasives")

# Load data
source("CURRENT_DATA.R")
load(gsub("Data/Envir/", "", npms_label))
## Load subset of plants of interest
load(gsub("Data/Envir/", "", interest_id_label))
## Load regression results
load(gsub("Data/", "", reg_label))

# load(npms_label)
# load(interest_id_label)
# load(reg_label)

library(dplyr)
library(lubridate)
library(tidyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)

# Selected taxa
# dic_id <- left_join(
#   sub_id, tax |> select(kew_id, taxon_name), by = "kew_id"
# )

# All taxa
dic_id <- d_plant |> select(kew_id, taxon_name_binom) |>
  mutate(taxon_name = taxon_name_binom, .keep = "unused") |> distinct()
## Kew id's
tot_kew_id <- dic_id |> pull(taxon_name)

# Filter to selected taxa
# tot_mat |> filter(taxon %in% dic_id$taxon_name, n > 5) |> 
#   pull(taxon) -> sel_taxa
# sel_kew_id <- dic_id |> filter(taxon_name %in% sel_taxa) |> pull(taxon_name)
# sel_kew_id <- dic_id |> pull(taxon_name)
#

#### UI ####

ui <- fluidPage(
  
  # App title
  titlePanel("Regression"),
  
  # Sidebar layout with input and output definitions
  fluidRow(
    
    column(
      2, "",
      mainPanel(
        # Input: Selector for
        # selectInput(
        #   "spec", "Species", sort(sel_kew_id)
        # ),
        selectizeInput(
          "spec", "Species", sort(tot_kew_id), options = list(create = TRUE)
        ),
        checkboxInput("pool_y", "Pool years", value = F),
        checkboxInput("n_trig", "Number of species", value = F)
      )
    ),
    column(
      5, "",
      mainPanel(
        plotOutput("n_plot", height = 320, width = 500),
      ),
      conditionalPanel(
        condition = "input.pool_y == 0 && input.n_trig == 0",
        uiOutput("plots_ind")
      ),
      conditionalPanel(
        condition = "input.pool_y == 1 && input.n_trig == 0",
        plotOutput("plots_pooled")
      ),
      conditionalPanel(
        condition = "input.pool_y == 0 && input.n_trig == 1",
        uiOutput("plots_ind_n")
      ),
      conditionalPanel(
        condition = "input.pool_y == 1 && input.n_trig == 1",
        plotOutput("plots_pooled_n")
      )
    ),
    column(
      5, "",
      conditionalPanel(
        condition = "input.pool_y == 0 && input.n_trig == 0",
        uiOutput("tabs_ind")
      ),
      conditionalPanel(
        condition = "input.pool_y == 1 && input.n_trig == 0",
        htmlOutput("tabs_pooled")
      ),
      conditionalPanel(
        condition = "input.pool_y == 0 && input.n_trig == 1",
        uiOutput("tabs_ind_n")
      ),
      conditionalPanel(
        condition = "input.pool_y == 1 && input.n_trig == 1",
        htmlOutput("tabs_pooled_n")
      )
    )
  )
)

#### SERVER ####

## FOR SOME REASON INDIVIDUAL PLOTS HAVE TO BE GENERATED IN A LOOP INDEXED
## FROM 2014 INSTEAD OF 2015 OTHERWISE 2015 DOES NOT SHOW

server <- function(input, output) {
  # Map species to kew_id
  kew_id <- reactive({
    dic_id |> filter(taxon_name == input$spec) |> pull(kew_id) |> 
      as.character()
  })
  
  # OUTPUT - PLOT of Number of observations ----
  output$n_plot <- renderPlot({
    # Create dataset
    n_year <- numeric()
    ## Find sample sizes that make sense
    for(j in 2015:2021){
      # Check that column exists
      test_col <- try(
        {pivot_reac(year = j, kew_local = kew_id()) |>
            select(kew_id(), ab_native)},
        silent = T
      )

      # Fill in columns
      if(inherits(test_col, "try-error")){next} else{
        n_year[j - 2014] <- test_col |> na.omit() |> nrow()
      }
    }

    # PLOT
    data.frame(year = as.character(2015:2021), n = n_year) |>
      ggplot(aes(x = as.numeric(year), y = n)) +
      geom_line() + xlab(paste(input$species)) +
      ggtitle("Number of locations per year") +
      ylab("Number of observations") + theme_bw() +
      theme(
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold")
      )
  })
  
  # OUTPUT - PLOTS ----
  ## POOLED
  ### ABUNDANCE
  output$plots_pooled <- renderPlot({
    ## Compute regression
    reg <- f_reg(kew_local = kew_id(), pool = T)
    ## Bind results
    reg_predict <- cbind(
      pooled_plant |> select(ab_native, kew_id()) |> filter(get(kew_id()) != 0),
      predict(reg, interval = 'confidence')
    )

    ggplot(reg_predict, aes(reg_predict[, paste(kew_id())], ab_native)) +
      geom_point() +
      geom_line(aes(reg_predict[, paste(kew_id())], fit)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
      xlab(paste(input$spec)) + ylab("Native abundance") +
      theme(
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold")
      ) + ggtitle("Pooled years")
  })
  ### NUMBER OF SPECIES
  output$plots_pooled_n <- renderPlot({
    ## Compute regression
    reg <- f_reg(kew_local = kew_id(), pool = T, no = T)
    ## Bind results
    reg_predict <- cbind(
      pooled_plant |> select(n_native, kew_id()) |> filter(get(kew_id()) != 0),
      predict(reg, interval = 'confidence')
    )
    
    ggplot(reg_predict, aes(reg_predict[, paste(kew_id())], n_native)) +
      geom_point() +
      geom_line(aes(reg_predict[, paste(kew_id())], fit)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
      xlab(paste(input$spec)) + ylab("Number of native species") +
      theme(
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold")
      ) + ggtitle("Pooled years")
  })
  ## INDIVIDUAL
  ### ABUNDANCE
  output$plots_ind <- renderUI({
    plot_output_list <- lapply(2014:2021, function(i) {
      # browser()
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 320, width = 500)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  for(i in 2014:2021){
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      # browser()
      # Function to actually render a plot
      plotname <- paste("plot", my_i, sep = "")
      output[[plotname]] <- renderPlot({
        # Calculate regression
        reg <- f_reg(year = my_i, kew_local = kew_id())
        
        # Bind results
        reg_predict <- cbind(
          pivot_reac(year = my_i, kew_local = kew_id()) |> 
            na.omit(), predict(reg, interval = 'confidence')
        )
        
        # Produce plot
        ggplot(reg_predict, aes(reg_predict[, paste(kew_id())], ab_native)) +
          geom_point() + 
          geom_line(aes(reg_predict[, paste(kew_id())], fit)) +
          geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
          xlab(paste(input$spec)) + ylab("Native abundance") +
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
        html_reg <- f_reg(year = my_i, kew_local = kew_id()) |>
          sjPlot::tab_model(dv.labels = paste(my_i))
        HTML(html_reg$page.content, html_reg$page.style)
      })
    })
  }
  ### NUMBER OF SPECIES
  output$plots_ind_n <- renderUI({
    plot_output_list <- lapply(2014:2021, function(i) {
      plotname <- paste("plot_n", i, sep="")
      plotOutput(plotname, height = 320, width = 500)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  for(i in 2014:2021){
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      
      # Function to actually render a plot
      plotname <- paste("plot_n", my_i, sep = "")
      output[[plotname]] <- renderPlot({
        # Calculate regression
        reg <- f_reg(year = my_i, kew_local = kew_id(), no = T)
        
        # Bind results
        reg_predict <- cbind(
          pivot_reac(year = my_i, kew_local = kew_id(), no = T) |> 
            na.omit(), predict(reg, interval = 'confidence')
        )
        
        # Produce plot
        ggplot(reg_predict, aes(reg_predict[, paste(kew_id())], n_native)) +
          geom_point() + 
          geom_line(aes(reg_predict[, paste(kew_id())], fit)) +
          geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + theme_bw() +
          xlab(paste(input$spec)) + ylab("Number of native species") +
          ggtitle(paste(my_i)) +
          theme(
            title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 18, face = "bold"),
            axis.text.y = element_text(size = 18, face = "bold")
          )
      })
      
      # Tables 
      tablename <- paste("table_n", my_i, sep = "")
      output[[tablename]] <- reactive({
        html_reg <- f_reg(year = my_i, kew_local = kew_id(), no = T) |>
          sjPlot::tab_model(dv.labels = paste(my_i))
        HTML(html_reg$page.content, html_reg$page.style)
      })
    })
  }
  
  # TABLES ----
  ## POOLED TABLE
  ### ABUNDANCE
  output$tabs_pooled <- reactive({
    html_reg <- f_reg(kew_local = kew_id(), pool = T) |>
      sjPlot::tab_model(dv.labels = "Pooled")
    HTML(html_reg$page.content, html_reg$page.style)
  })
  output$tabs_pooled_n <- reactive({
    html_reg <- f_reg(kew_local = kew_id(), pool = T, no = T) |>
      sjPlot::tab_model(dv.labels = "Pooled")
    HTML(html_reg$page.content, html_reg$page.style)
  })
  ### NUMBER OF SPECIES
  ## INDIVIDUAL
  ### ABUNDANCE
  output$tabs_ind <- renderUI({
    table_output_list <- lapply(2014:2021, function(i) {
      tablename <- paste("table", i, sep="")
      htmlOutput(tablename)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, table_output_list)
  })
  ### NUMBER OF SPECIES
  output$tabs_ind_n <- renderUI({
    table_output_list <- lapply(2014:2021, function(i) {
      tablename <- paste("table_n", i, sep="")
      htmlOutput(tablename)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, table_output_list)
  })
  
  # General functions ----
  ## Map species to kew_id
  kew_id <- reactive({
    dic_id |> filter(taxon_name == input$spec) |> pull(kew_id) |> 
      as.character()
  })
  ## Data
  pivot_reac <- function(year, kew_local, no = F){
    # Id's for adding up native abundance
    id_native <- d_plant |> 
      filter(StaceIV_nativity == "N", year(date) == year) |>
      pull(kew_id) |> unique()
    ## Filter dataset
    filtered_data <- d_plant |> 
      filter(is.na(StaceIV_nativity) == F, year(date) == year) |>
      distinct(kew_id, location_id, .keep_all = T) |>
      select(kew_id, midPoint, location_id) |>
      pivot_wider(
        names_from = kew_id, values_from = midPoint, values_fill = NA
      ) |> mutate(
        ab_native = rowSums(across(.cols = contains(id_native)), na.rm = T),
        n_native = rowSums(is.na(across(.cols = contains(id_native))) == F)
      )
    
    # Pivot data
    if(no == T){
      filtered_data |> select(n_native, kew_local) |> as.data.frame()
    } else{
      filtered_data |> select(ab_native, kew_local) |> as.data.frame()
    }
  }
  ## Regression
  f_reg <- function(year, kew_local, pool = F, no = F){
    # browser()
    if(pool == T){
      ## Pooled years
      if(no == T){
        lm(data = pooled_plant |> select(n_native, kew_local) |> 
             filter(get(kew_local) != 0) |> as.data.frame(), 
           n_native ~ .)
      } else{
        lm(data = pooled_plant |> select(ab_native, kew_local) |> 
             filter(get(kew_local) != 0) |> as.data.frame(), 
           ab_native ~ .)
      }
    } else{
      ## Individual year
      if(no == T){
        lm(
          data = pivot_reac(year = year, kew_local = kew_local, no = T), 
          n_native ~ .
        )
      } else{
        lm(
          data = pivot_reac(year = year, kew_local = kew_local), ab_native ~ .
        )
      }
    }
  }
}

shinyApp(ui, server)
#