# US Superstore Explorer (based on app_prep.qmd).

# Required packages/libraries.
library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(shinycssloaders)

theme_set(theme_minimal(base_size = 12))

# Import and clean names with tidyverse.
data_path <- "US Superstore data.xls"
if (!file.exists(data_path)) {
  stop('Place "US Superstore data.xls" in the same folder as app.R.')
}
superstore <- read_excel(data_path, sheet = "Orders") |>
  rename_with(~ str_replace_all(str_to_lower(.x), "[ -]+", "_")) |>
  mutate(
    order_date = as.Date(order_date),
    ship_date = as.Date(ship_date),
    postal_code = str_pad(as.character(postal_code), 5, pad = "0"),
    profit_status = case_when(
      is.na(profit) ~ "Missing",
      profit > 0 ~ "Profit",
      profit < 0 ~ "Loss",
      TRUE ~ "Break-even"
    )
  )

numeric_choices <- c(Sales = "sales", Profit = "profit",
                     Quantity = "quantity", Discount = "discount")
category_choices <- c(Category = "category", Subcategory = "sub_category",
                      Region = "region", Segment = "segment",
                      "Ship mode" = "ship_mode", "Profit status" = "profit_status")
regions <- sort(unique(superstore$region))
categories <- sort(unique(superstore$category))

# Helpers for readable labels, units, and tables.
pretty_name <- function(x) str_to_title(str_replace_all(x, "_", " "))
axis_title <- function(x) {
  switch(x, sales = "Sales per order line (USD)",
         profit = "Profit per order line (USD)",
         quantity = "Units per order line", discount = "Discount")
}
number_labels <- function(x) {
  if (x %in% c("sales", "profit")) scales::label_dollar()
  else if (x == "discount") scales::label_percent()
  else scales::label_number()
}
show_table <- function(x) {
  result <- DT::datatable(x, rownames = FALSE,
                          options = list(pageLength = 10, scrollX = TRUE, order = list()))
  numeric_columns <- names(x)[vapply(x, is.numeric, logical(1))]
  # Keep counts as integers and round other numerical summaries.
  rounded <- setdiff(numeric_columns, c("order_lines", "units_sold", "n", "missing"))
  if (length(rounded)) result <- DT::formatRound(result, rounded, digits = 2)
  result
}

ui <- fluidPage(
  titlePanel("US Superstore Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter the data"),
      checkboxGroupInput("regions", "Region", regions, selected = regions),
      actionButton("all_regions", "Select all regions"),
      checkboxGroupInput("categories", "Category", categories, selected = categories),
      actionButton("all_categories", "Select all categories"),
      hr(),
      selectInput("filter_num1", "First numeric filter", numeric_choices, "sales"),
      uiOutput("slider1"),
      selectInput("filter_num2", "Second numeric filter", numeric_choices, "profit"),
      uiOutput("slider2"),
      helpText("Full slider ranges keep all values. Discount is a proportion: 0.20 means 20%."),
      actionButton("apply", "Apply filters", class = "btn-primary"),
      helpText("Sidebar changes take effect only after you click Apply filters."),
      textOutput("filter_status")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        selected = "About",
        tabPanel(
          "About",
          h3("Explore sales, profit, and purchasing activity"),
          p("Compare product categories, customer segments, and regions using tables and graphs based on the accompanying exploratory Quarto document."),
          tags$figure(
            class = "centerFigure",
            style = "text-align: center;",
            tags$img(src = "store_pixabay.png", width = 600,
                     style = "max-width: 100%; height: auto;", alt = "Online store graphic"),
            tags$figcaption("Online Store Graphic from Pixabay")
          ),
          h4("About the data"),
          p(paste0("The workbook contains ", format(nrow(superstore), big.mark = ","),
                   " order lines across ", format(n_distinct(superstore$order_id), big.mark = ","),
                   " orders, from ", min(superstore$order_date), " to ",
                   max(superstore$order_date), ".")),
          p("Each row is an order line, which may include multiple units. Sales and profit are dollar amounts; negative profit indicates a loss. Order-line counts are not counts of unique customers or orders."),
          p("Source: ", tags$a("US Superstore dataset on Kaggle",
                               href = "https://www.kaggle.com/datasets/juhi1994/superstore", rel = "noopener")),
          h4("How to use this app"),
          tags$ul(
            tags$li("Sidebar: select regions, categories, and two numeric ranges, then click Apply filters. Initially, all data are displayed."),
            tags$li("Data Download: view and download the currently applied subset."),
            tags$li("Data Exploration: choose categorical or numeric summaries, compare revenue and profit, or create any of the six plot types."),
            tags$li("Exploration controls update immediately using the applied subset. Select grouping, color, and facet variables to compare groups.")
          ),
          p("These summaries describe this dataset. Associations in the plots do not establish cause and effect.")
        ),
        tabPanel(
          "Data Download",
          h3("Applied data subset"),
          downloadButton("download", "Download CSV"),
          helpText("The CSV includes the entire applied subset. Table search and pagination only affect the on-screen display."),
          withSpinner(DT::dataTableOutput("data_table"))
        ),
        tabPanel(
          "Data Exploration",
          tabsetPanel(
            tabPanel(
              "Summary Tables",
              radioButtons("summary_type", "Summary type",
                           c("Categorical", "Numeric"), inline = TRUE),
              conditionalPanel(
                "input.summary_type === 'Categorical'",
                radioButtons("table_type", "Table type", c("One-way", "Two-way"), inline = TRUE),
                selectInput("table_cat1", "Categorical variable / rows", category_choices, "category"),
                conditionalPanel(
                  "input.table_type === 'Two-way'",
                  selectInput("table_cat2", "Second categorical variable / columns", category_choices, "region"),
                  selectInput("table_measure", "Display", c("Counts", "Row percentages", "Column percentages"))
                )
              ),
              conditionalPanel(
                "input.summary_type === 'Numeric'",
                selectInput("summary_num", "Numeric variable", numeric_choices, "profit"),
                selectInput("summary_group", "Group by", category_choices, "category"),
                selectInput("summary_group2", "Optional second group", c(None = "none", category_choices)),
                helpText("Means, medians, and standard deviations describe values per order line. Discounts are proportions; summing them is not meaningful.")
              ),
              withSpinner(DT::dataTableOutput("summary_table"))
            ),
            tabPanel(
              "Revenue and Profit",
              selectInput("performance_group", "Compare",
                          c(Category = "category", Subcategory = "sub_category",
                            Product = "product", Region = "region", Segment = "segment",
                            "Category and region" = "category_region")),
              selectInput("rank_by", "Rank by", c("Total revenue" = "total_revenue",
                                                  "Total profit" = "total_profit", "Units sold" = "units_sold")),
              radioButtons("rank_direction", "Order", c("Highest first", "Lowest first"), inline = TRUE),
              selectInput("rank_limit", "Show", c("All" = "all", "Top/bottom 10" = "10", "Top/bottom 20" = "20")),
              checkboxInput("profit_matrix", "Show category-region total profit as a wide table", FALSE),
              withSpinner(DT::dataTableOutput("performance_table"))
            ),
            tabPanel(
              "Plots",
              selectInput("plot_type", "Plot type",
                          c("Bar chart", "Side-by-side bar chart", "Histogram",
                            "Box plot", "Scatter plot", "Heatmap")),
              fluidRow(
                column(6,
                       conditionalPanel(
                         "['Bar chart','Side-by-side bar chart','Box plot','Heatmap'].includes(input.plot_type)",
                         selectInput("plot_cat1", "First categorical variable", category_choices, "category")
                       ),
                       conditionalPanel(
                         "['Side-by-side bar chart','Heatmap'].includes(input.plot_type)",
                         selectInput("plot_cat2", "Second categorical variable", category_choices, "region")
                       ),
                       conditionalPanel(
                         "['Histogram','Box plot','Scatter plot','Heatmap'].includes(input.plot_type)",
                         selectInput("plot_num1", "Numeric variable (scatter plot: X)", numeric_choices, "sales")
                       ),
                       conditionalPanel(
                         "input.plot_type === 'Scatter plot'",
                         selectInput("plot_num2", "Y variable", numeric_choices, "profit")
                       ),
                       conditionalPanel(
                         "input.plot_type === 'Heatmap'",
                         selectInput("heat_stat", "Heatmap statistic", c("Mean", "Median", "Total"))
                       )
                ),
                column(6,
                       conditionalPanel(
                         "['Bar chart','Histogram','Box plot','Scatter plot'].includes(input.plot_type)",
                         selectInput("plot_color", "Color by", c(None = "none", category_choices))
                       ),
                       selectInput("plot_facet", "Facet by", c(None = "none", category_choices)),
                       conditionalPanel(
                         "input.plot_type === 'Histogram'",
                         sliderInput("bins", "Histogram bins", 5, 60, 30),
                         checkboxInput("log_scale", "Use logarithmic X axis", FALSE)
                       ),
                       conditionalPanel(
                         "input.plot_type === 'Box plot'",
                         checkboxInput("zoom_profit", "Zoom profit axis to -100 through 200 USD", FALSE)
                       )
                )
              ),
              withSpinner(plotOutput("exploration_plot", height = "650px")),
              helpText("To reproduce the QMD: histogram = Sales with a log axis and Category facets; box plot = Profit by Category with zoom; scatter = Discount vs Profit colored by Category; heatmap = mean Profit by Category and Region.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$all_regions, {
    updateCheckboxGroupInput(session, "regions", selected = regions)
  })
  observeEvent(input$all_categories, {
    updateCheckboxGroupInput(session, "categories", selected = categories)
  })
  
  # Dynamic range sliders use variable-specific IDs to avoid stale values
  # from the old variable while a new slider is being created.
  make_slider <- function(variable, prefix) {
    limits <- range(superstore[[variable]], na.rm = TRUE)
    step <- if (variable == "quantity") 1 else 0.01
    limits <- c(floor(limits[1] / step) * step, ceiling(limits[2] / step) * step)
    sliderInput(paste0(prefix, variable), axis_title(variable),
                min = limits[1], max = limits[2], value = limits, step = step)
  }
  output$slider1 <- renderUI({ req(input$filter_num1); make_slider(input$filter_num1, "range1_") })
  output$slider2 <- renderUI({ req(input$filter_num2); make_slider(input$filter_num2, "range2_") })
  
  # Only the Apply button changes the data used by outputs.
  applied <- reactiveValues(data = superstore, clicks = 0)
  observeEvent(input$apply, {
    bounds1 <- input[[paste0("range1_", input$filter_num1)]]
    bounds2 <- input[[paste0("range2_", input$filter_num2)]]
    if (length(bounds1) != 2 || length(bounds2) != 2) {
      showNotification("Wait for both sliders to load, then apply again.", type = "message")
      return()
    }
    applied$data <- superstore |>
      filter(region %in% input$regions, category %in% input$categories,
             between(.data[[input$filter_num1]], bounds1[1], bounds1[2]),
             between(.data[[input$filter_num2]], bounds2[1], bounds2[2]))
    applied$clicks <- applied$clicks + 1
  })
  filtered_data <- reactive(applied$data)
  exploration_data <- reactive({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No matching order lines. Widen the filters and click Apply filters."))
    data
  })
  output$filter_status <- renderText({
    paste0(if (applied$clicks == 0) "Initial data: " else "Applied subset: ",
           format(nrow(filtered_data()), big.mark = ","), " order lines; ",
           format(n_distinct(filtered_data()$order_id), big.mark = ","), " unique orders.")
  })
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  output$download <- downloadHandler(
    filename = function() paste0("superstore_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered_data(), file)
  )
  
  # Generalize the QMD's one-way and two-way contingency tables.
  output$summary_table <- DT::renderDataTable({
    data <- exploration_data()
    if (input$summary_type == "Categorical") {
      first <- input$table_cat1
      if (input$table_type == "One-way") {
        result <- data |>
          count(.data[[first]], name = "order_lines") |>
          mutate(percent = 100 * order_lines / sum(order_lines))
      } else {
        second <- input$table_cat2
        validate(need(first != second, "Choose two different categorical variables."))
        result <- data |> count(.data[[first]], .data[[second]], name = "value")
        if (input$table_measure == "Row percentages") {
          result <- result |> group_by(.data[[first]]) |>
            mutate(value = 100 * value / sum(value)) |> ungroup()
        } else if (input$table_measure == "Column percentages") {
          result <- result |> group_by(.data[[second]]) |>
            mutate(value = 100 * value / sum(value)) |> ungroup()
        }
        result <- result |> pivot_wider(names_from = all_of(second), values_from = value, values_fill = 0)
      }
    } else {
      variable <- input$summary_num
      groups <- unique(c(input$summary_group, if (input$summary_group2 != "none") input$summary_group2))
      result <- data |> group_by(across(all_of(groups))) |>
        summarise(order_lines = n(), missing = sum(is.na(.data[[variable]])),
                  mean = mean(.data[[variable]], na.rm = TRUE),
                  median = median(.data[[variable]], na.rm = TRUE),
                  sd = sd(.data[[variable]], na.rm = TRUE),
                  minimum = min(.data[[variable]], na.rm = TRUE),
                  maximum = max(.data[[variable]], na.rm = TRUE),
                  total = sum(.data[[variable]], na.rm = TRUE), .groups = "drop")
      if (variable == "discount") result <- result |> select(-total)
    }
    show_table(result)
  })
  
  # Preserve the category, subcategory, product, and category-region analyses.
  output$performance_table <- DT::renderDataTable({
    data <- exploration_data()
    if (input$profit_matrix) {
      result <- data |> group_by(category, region) |>
        summarise(total_profit = sum(profit), .groups = "drop") |>
        pivot_wider(names_from = region, values_from = total_profit)
    } else {
      groups <- switch(input$performance_group,
                       sub_category = c("category", "sub_category"),
                       product = c("product_id", "product_name"),
                       category_region = c("category", "region"), input$performance_group)
      result <- data |> group_by(across(all_of(groups))) |>
        summarise(order_lines = n(), units_sold = sum(quantity),
                  total_revenue = sum(sales), total_profit = sum(profit),
                  mean_profit = mean(profit), median_profit = median(profit), sd_profit = sd(profit),
                  mean_sales = mean(sales), median_sales = median(sales), sd_sales = sd(sales),
                  .groups = "drop")
      if (input$rank_direction == "Highest first") {
        result <- result |> arrange(desc(.data[[input$rank_by]]))
      } else result <- result |> arrange(.data[[input$rank_by]])
      if (input$rank_limit != "all") result <- result |> slice_head(n = as.integer(input$rank_limit))
    }
    show_table(result)
  })
  
  # Set defaults matching the QMD whenever the plot type changes.
  observeEvent(input$plot_type, {
    type <- input$plot_type
    updateSelectInput(session, "plot_color", selected = if (type == "Scatter plot") "category" else "none")
    updateSelectInput(session, "plot_facet", selected = if (type == "Histogram") "category" else "none")
    updateSelectInput(session, "plot_cat1", selected = if (type %in% c("Side-by-side bar chart", "Heatmap")) "region" else "category")
    updateSelectInput(session, "plot_cat2", selected = if (type %in% c("Side-by-side bar chart", "Heatmap")) "category" else "region")
    updateSelectInput(session, "plot_num1", selected = switch(type,
                                                              "Scatter plot" = "discount", "Box plot" = "profit", "Heatmap" = "profit", "sales"))
    updateSelectInput(session, "plot_num2", selected = "profit")
    updateCheckboxInput(session, "log_scale", value = type == "Histogram")
    updateCheckboxInput(session, "zoom_profit", value = type == "Box plot")
  })
  
  output$exploration_plot <- renderPlot({
    data <- exploration_data()
    req(input$plot_type, input$plot_cat1, input$plot_cat2,
        input$plot_num1, input$plot_num2, input$plot_color, input$plot_facet)
    type <- input$plot_type
    cat1 <- input$plot_cat1; cat2 <- input$plot_cat2
    num1 <- input$plot_num1; num2 <- input$plot_num2
    color <- input$plot_color; facet <- input$plot_facet
    
    if (type == "Bar chart") {
      p <- ggplot(data, aes(x = .data[[cat1]]))
      if (color == "none") p <- p + geom_bar(fill = "navyblue")
      else p <- p + geom_bar(aes(fill = .data[[color]]))
      p <- p + labs(title = paste("Order lines by", pretty_name(cat1)),
                    x = pretty_name(cat1), y = "Number of order lines", fill = pretty_name(color))
    } else if (type == "Side-by-side bar chart") {
      validate(need(cat1 != cat2, "Choose two different categorical variables."))
      p <- ggplot(data, aes(x = .data[[cat1]], fill = .data[[cat2]])) +
        geom_bar(position = "dodge") +
        labs(title = paste("Order lines by", pretty_name(cat1), "and", pretty_name(cat2)),
             x = pretty_name(cat1), y = "Number of order lines", fill = pretty_name(cat2))
    } else if (type == "Histogram") {
      p <- ggplot(data, aes(x = .data[[num1]]))
      if (color == "none") p <- p + geom_histogram(bins = input$bins, fill = "navyblue", color = "white")
      else p <- p + geom_histogram(aes(fill = .data[[color]]), bins = input$bins, position = "identity", alpha = 0.5)
      if (input$log_scale) {
        validate(need(all(data[[num1]] > 0), "Logarithmic axes require positive values. Turn off the log axis or choose a positive variable."))
        p <- p + scale_x_log10(labels = number_labels(num1)) +
          labs(subtitle = "Logarithmic X axis; equal-width bins in log10 values")
      } else p <- p + scale_x_continuous(labels = number_labels(num1))
      p <- p + labs(title = paste("Distribution of", pretty_name(num1)),
                    x = axis_title(num1), y = "Number of order lines", fill = pretty_name(color))
    } else if (type == "Box plot") {
      p <- ggplot(data, aes(x = .data[[cat1]], y = .data[[num1]]))
      if (num1 == "profit") p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
      if (color == "none") p <- p + geom_boxplot(fill = "turquoise", outlier.alpha = 0.25)
      else p <- p + geom_boxplot(aes(fill = .data[[color]]), outlier.alpha = 0.25)
      p <- p + scale_y_continuous(labels = number_labels(num1)) +
        labs(title = paste(pretty_name(num1), "by", pretty_name(cat1)),
             x = pretty_name(cat1), y = axis_title(num1), fill = pretty_name(color))
      if (input$zoom_profit && num1 == "profit") {
        p <- p + coord_cartesian(ylim = c(-100, 200)) +
          labs(subtitle = "Zoomed to -100 through 200 USD; box statistics use all applied rows")
      }
    } else if (type == "Scatter plot") {
      validate(need(num1 != num2, "Choose different X and Y variables."))
      p <- ggplot(data, aes(x = .data[[num1]], y = .data[[num2]]))
      if (num2 == "profit") p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
      if (color == "none") p <- p + geom_point(alpha = 0.25, size = 1, color = "navyblue")
      else p <- p + geom_point(aes(color = .data[[color]]), alpha = 0.25, size = 1)
      p <- p + scale_x_continuous(labels = number_labels(num1)) +
        scale_y_continuous(labels = number_labels(num2)) +
        labs(title = paste(pretty_name(num1), "and", pretty_name(num2)),
             x = axis_title(num1), y = axis_title(num2), color = pretty_name(color),
             caption = "Association does not establish causation.")
    } else {
      validate(need(cat1 != cat2, "Choose two different categorical variables."),
               need(!(num1 == "discount" && input$heat_stat == "Total"),
                    "Choose mean or median for discount; total discount is not meaningful."))
      groups <- unique(c(cat1, cat2, if (facet != "none") facet))
      statistic <- switch(input$heat_stat, Mean = mean, Median = median, Total = sum)
      plot_data <- data |> group_by(across(all_of(groups))) |>
        summarise(value = statistic(.data[[num1]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(plot_data, aes(x = .data[[cat1]], y = .data[[cat2]], fill = value)) +
        geom_tile(color = "white", linewidth = 1) +
        scale_fill_gradient2(low = "yellow", mid = "white", high = "orange", midpoint = 0,
                             labels = number_labels(num1)) +
        labs(title = paste(input$heat_stat, pretty_name(num1), "by", pretty_name(cat1), "and", pretty_name(cat2)),
             x = pretty_name(cat1), y = pretty_name(cat2),
             fill = paste(input$heat_stat, pretty_name(num1)))
      if (nrow(plot_data) <= 80) {
        label_function <- if (num1 %in% c("sales", "profit")) scales::label_dollar(accuracy = 0.01)
        else if (num1 == "discount") scales::label_percent(accuracy = 0.1)
        else scales::label_number(accuracy = 0.01)
        p <- p + geom_text(aes(label = label_function(value)), size = 3.5, color = "black")
      }
    }
    # Faceting remains selectable for every plot type.
    if (facet != "none") p <- p + facet_wrap(vars(!!rlang::sym(facet)), ncol = 1)
    p + theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.position = "bottom", plot.title = element_text(face = "bold"))
  })
}

shinyApp(ui = ui, server = server)
