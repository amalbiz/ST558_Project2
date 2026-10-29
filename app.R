# Load packages
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)

# Import data once when the app starts
superstore <- read_excel("US Superstore data.xls", sheet = "Orders") |>
  clean_names() |>
  mutate(
    order_date = as.Date(order_date),
    ship_date = as.Date(ship_date),
    postal_code = str_pad(as.character(postal_code), 5, pad = "0")
  )

# User Interface
ui <- fluidPage(
  titlePanel("US Superstore Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Let users select one or more regions.
      # All regions are selected initially.
      checkboxGroupInput(
        "region",
        "Choose regions:",
        choices = sort(unique(superstore$region)),
        selected = sort(unique(superstore$region))
      ),
      
      # Let users select one or more product categories.
      checkboxGroupInput(
        "category",
        "Choose product categories:",
        choices = sort(unique(superstore$category)),
        selected = sort(unique(superstore$category))
      ),
      
      # Changes take effect only after this button is clicked.
      actionButton("apply", "Apply filters")
    ),
    
    mainPanel(
      tabsetPanel(
        # Open the About tab by default.
        selected = "About",
        
        tabPanel(
          "About",
          h3("About this app"),
          p("Explore sales, profit, and purchasing patterns in US Superstore data."),
          p("Each row represents a product line within an order."),
          p("Choose filters in the sidebar, then click Apply filters."),
          p("Data Download will display and export the filtered data."),
          p("Data Exploration will provide tables and plots."),
          tags$a(
            href = "https://www.kaggle.com/datasets/juhi1994/superstore",
            "View the dataset source",
            target = "_blank"
          )
        ),
        
        tabPanel(
          "Data Download",
          p("The interactive data table and download button will go here.")
        ),
        
        tabPanel(
          "Data Exploration",
          plotOutput("category_plot")
        )
      )
    )
  )
) 

# SERVER
server <- function(input, output, session) {
  
  # Create the filtered dataset when Apply filters is clicked.
  # ignoreNULL = FALSE also creates the initial dataset at startup.
  filtered_data <- eventReactive(input$apply, {
    superstore |>
      filter(
        region %in% input$region,
        category %in% input$category
      )
  }, ignoreNULL = FALSE)
  
  # Display a bar chart using the applied filters.
  output$category_plot <- renderPlot({
    data <- filtered_data()
    
    # Show a useful message if the filters return no rows.
    validate(
      need(nrow(data) > 0, "Select at least one region and category, then click Apply filters.")
    )
    
    ggplot(data, aes(x = category)) +
      geom_bar(fill = "navyblue") +
      labs(
        title = "Order lines by product category",
        x = "Product category",
        y = "Number of order lines"
      ) +
      theme_minimal()
  })
}

# Launch the app
shinyApp(ui = ui, server = server)