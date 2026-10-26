# 1. Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)
library(corrplot)

# 2. Load dataset
file_name <- "AirQualityUCI.csv"

if (!file.exists(file_name)) {
  stop("Please put AirQualityUCI.csv in the same folder as app.R")
}

# The original UCI file commonly uses semicolons and decimal commas
data <- read.csv2(file_name, stringsAsFactors = FALSE,
                  na.strings = c("", "NA"))

# Fallback for a comma-separated CSV
if (ncol(data) < 5) {
  data <- read.csv(file_name, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"), check.names = FALSE)
}

# Remove empty columns sometimes found in the original file
data <- data[, !grepl("^X$|^X\\.[0-9]+$|^Unnamed", names(data)),
             drop = FALSE]

# Standardize column names
names(data) <- make.names(trimws(names(data)), unique = TRUE)

if (!all(c("Date", "Time") %in% names(data))) {
  stop("Date and Time columns were not found. Check the dataset file.")
}

# 3. Convert numeric columns, including decimal-comma values
for (nm in names(data)) {
  if (!nm %in% c("Date", "Time") && is.character(data[[nm]])) {
    converted <- suppressWarnings(
      as.numeric(gsub(",", ".", data[[nm]], fixed = TRUE))
    )
    
    if (sum(!is.na(converted)) > 0.5 * length(converted)) {
      data[[nm]] <- converted
    }
  }
}

# 4. Create timestamp from Date and Time
date_text <- trimws(as.character(data$Date))
time_text <- gsub("\\.", ":", trimws(as.character(data$Time)))

timestamp <- suppressWarnings(
  dmy_hms(paste(date_text, time_text), quiet = TRUE)
)

if (all(is.na(timestamp))) {
  timestamp <- as.POSIXct(
    paste(date_text, time_text),
    format = "%d/%m/%Y %H:%M:%S",
    tz = "UTC"
  )
}

if (all(is.na(timestamp))) {
  stop("Could not read Date and Time. Check the CSV format.")
}

data$Timestamp <- timestamp
data <- data %>% filter(!is.na(Timestamp))
data$DateOnly <- as.Date(data$Timestamp)
data$MonthDate <- floor_date(data$Timestamp, "month")

# 5. Handle missing values
# In this dataset, -200 represents a missing measurement
numeric_cols <- names(data)[sapply(data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, character(0))

for (nm in numeric_cols) {
  data[[nm]][data[[nm]] == -200] <- NA
}

# 6. Identify available numeric variables
preferred_variables <- c(
  "CO.GT.", "PT08.S1.CO.", "NMHC.GT.", "C6H6.GT.",
  "PT08.S2.NMHC.", "NOx.GT.", "PT08.S3.NOx.",
  "NO2.GT.", "PT08.S4.NO2.", "PT08.S5.O3.",
  "T", "RH", "AH"
)

variables <- intersect(
  preferred_variables,
  names(data)[sapply(data, is.numeric)]
)

if (length(variables) < 2) {
  stop("Expected numeric pollution variables were not found in the CSV.")
}

# Friendly labels for charts
variable_labels <- c(
  "CO.GT." = "Carbon Monoxide (CO)",
  "PT08.S1.CO." = "CO Sensor Response",
  "NMHC.GT." = "Non-Methane Hydrocarbons",
  "C6H6.GT." = "Benzene (C6H6)",
  "PT08.S2.NMHC." = "NMHC Sensor Response",
  "NOx.GT." = "Nitrogen Oxides (NOx)",
  "PT08.S3.NOx." = "NOx Sensor Response",
  "NO2.GT." = "Nitrogen Dioxide (NO2)",
  "PT08.S4.NO2." = "NO2 Sensor Response",
  "PT08.S5.O3." = "Ozone Sensor Response",
  "T" = "Temperature",
  "RH" = "Relative Humidity",
  "AH" = "Absolute Humidity"
)

label_var <- function(x) {
  if (x %in% names(variable_labels)) {
    unname(variable_labels[x])
  } else {
    x
  }
}

variable_choices <- setNames(
  variables,
  vapply(variables, label_var, character(1))
)

default_var <- if ("CO.GT." %in% variables) "CO.GT." else variables[1]
second_var <- if ("C6H6.GT." %in% variables) {
  "C6H6.GT."
} else {
  variables[2]
}

# ============================================================
# 7. DASHBOARD USER INTERFACE
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Air Quality Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview",
               icon = icon("dashboard")),
      menuItem("Time Trends", tabName = "trends",
               icon = icon("line-chart")),
      menuItem("Monthly Analysis", tabName = "monthly",
               icon = icon("calendar")),
      menuItem("Relationships", tabName = "relationships",
               icon = icon("scatter-chart")),
      menuItem("Correlation Heatmap", tabName = "correlation",
               icon = icon("th")),
      menuItem("Data Explorer", tabName = "data",
               icon = icon("table"))
    ),
    
    hr(),
    
    dateRangeInput(
      "date_range", "Select date range",
      start = min(data$DateOnly, na.rm = TRUE),
      end = max(data$DateOnly, na.rm = TRUE),
      min = min(data$DateOnly, na.rm = TRUE),
      max = max(data$DateOnly, na.rm = TRUE)
    ),
    
    selectInput(
      "variable", "Select variable",
      choices = variable_choices,
      selected = default_var
    ),
    
    selectInput(
      "variable2", "Select second variable",
      choices = variable_choices,
      selected = second_var
    ),
    
    downloadButton("download_data", "Download filtered data")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .box { border-radius: 6px; }
    "))),
    
    tabItems(
      
      # ---------------- OVERVIEW ----------------
      tabItem(
        tabName = "overview",
        
        h2("Air Quality and Pollution Trend Analysis"),
        
        p("Explore air pollution patterns, monthly changes,
          distributions, and relationships between variables.
          Use the filters in the sidebar to update the dashboard."),
        
        fluidRow(
          valueBoxOutput("record_box", width = 4),
          valueBoxOutput("variable_box", width = 4),
          valueBoxOutput("missing_box", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Pollution Trend",
            status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("overview_trend", height = 300)
          ),
          
          box(
            title = "Summary Statistics",
            status = "primary", solidHeader = TRUE, width = 4,
            DTOutput("summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Monthly Average",
            status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("overview_monthly", height = 300)
          ),
          
          box(
            title = "Distribution of Measurements",
            status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("overview_histogram", height = 300)
          )
        )
      ),
      
      # ---------------- TIME TRENDS ----------------
      tabItem(
        tabName = "trends",
        
        h2("Pollution Trends Over Time"),
        
        box(
          title = "Daily Average Trend",
          status = "primary", solidHeader = TRUE, width = 12,
          plotlyOutput("daily_trend", height = 420)
        ),
        
        box(
          title = "Daily Summary Table",
          status = "info", solidHeader = TRUE, width = 12,
          DTOutput("daily_table")
        )
      ),
      
      # ---------------- MONTHLY ANALYSIS ----------------
      tabItem(
        tabName = "monthly",
        
        h2("Monthly Pollution Analysis"),
        
        fluidRow(
          box(
            title = "Monthly Average",
            status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("monthly_average", height = 350)
          ),
          
          box(
            title = "Monthly Distribution",
            status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("monthly_boxplot", height = 350)
          )
        ),
        
        box(
          title = "Monthly Statistics",
          status = "info", solidHeader = TRUE, width = 12,
          DTOutput("monthly_table")
        )
      ),
      
      # ---------------- RELATIONSHIPS ----------------
      tabItem(
        tabName = "relationships",
        
        h2("Relationships Between Variables"),
        
        box(
          title = "Scatter Plot with Trend Line",
          status = "primary", solidHeader = TRUE, width = 12,
          plotlyOutput("scatter_plot", height = 450)
        ),
        
        p("A scatter plot helps identify whether two variables
          increase together, decrease together, or show no clear
          linear relationship. Association does not prove causation.")
      ),
      
      # ---------------- CORRELATION ----------------
      tabItem(
        tabName = "correlation",
        
        h2("Correlation Heatmap"),
        
        box(
          title = "Correlation Between Numeric Variables",
          status = "primary", solidHeader = TRUE, width = 12,
          plotOutput("correlation_plot", height = 600)
        ),
        
        p("Correlation values range from -1 to +1. Values near +1
          indicate a positive linear association, values near -1
          indicate a negative association, and values near 0
          indicate a weak linear association.")
      ),
      
      # ---------------- DATA EXPLORER ----------------
      tabItem(
        tabName = "data",
        
        h2("Cleaned Data Explorer"),
        
        box(
          title = "Filtered Dataset",
          status = "primary", solidHeader = TRUE, width = 12,
          DTOutput("data_table")
        )
      )
    )
  )
)

# ============================================================
# 8. DASHBOARD SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Apply selected date range
  filtered_data <- reactive({
    req(input$date_range)
    
    data %>%
      filter(
        DateOnly >= as.Date(input$date_range[1]),
        DateOnly <= as.Date(input$date_range[2])
      )
  })
  
  # Selected variable with valid measurements
  selected_data <- reactive({
    req(input$variable)
    
    filtered_data() %>%
      transmute(
        Timestamp,
        DateOnly,
        MonthDate,
        Value = .data[[input$variable]]
      ) %>%
      filter(!is.na(Value), is.finite(Value))
  })
  
  # ---------------- KPI BOXES ----------------
  
  output$record_box <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()), big.mark = ","),
      "Records in selected date range",
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  output$variable_box <- renderValueBox({
    valueBox(
      length(variables),
      "Available numeric variables",
      icon = icon("sliders"),
      color = "green"
    )
  })
  
  output$missing_box <- renderValueBox({
    fd <- filtered_data()
    missing_count <- sum(is.na(fd[, variables, drop = FALSE]))
    
    valueBox(
      format(missing_count, big.mark = ","),
      "Missing pollutant/weather values",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  # ---------------- SUMMARY TABLE ----------------
  
  output$summary_table <- renderDT({
    x <- selected_data()$Value
    
    stats <- data.frame(
      Statistic = c(
        "Valid observations", "Mean", "Median",
        "Standard deviation", "Minimum", "Maximum"
      ),
      Value = c(
        length(x),
        mean(x),
        median(x),
        if (length(x) > 1) sd(x) else NA_real_,
        min(x),
        max(x)
      )
    )
    
    datatable(
      stats,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE)
    ) %>%
      formatRound("Value", digits = 3)
  })
  
  # ---------------- OVERVIEW TREND ----------------
  
  output$overview_trend <- renderPlotly({
    sd <- selected_data()
    
    p <- ggplot(sd, aes(Timestamp, Value)) +
      geom_line(color = "#0072B2", linewidth = 0.5) +
      labs(
        x = "Date and time",
        y = label_var(input$variable)
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- OVERVIEW MONTHLY ----------------
  
  output$overview_monthly <- renderPlotly({
    md <- selected_data() %>%
      group_by(MonthDate) %>%
      summarise(Average = mean(Value), .groups = "drop")
    
    p <- ggplot(md, aes(MonthDate, Average)) +
      geom_col(fill = "#009E73") +
      labs(
        x = "Month",
        y = paste("Average", label_var(input$variable))
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- HISTOGRAM ----------------
  
  output$overview_histogram <- renderPlotly({
    sd <- selected_data()
    
    p <- ggplot(sd, aes(Value)) +
      geom_histogram(
        bins = 30,
        fill = "#E69F00",
        color = "white"
      ) +
      labs(
        x = label_var(input$variable),
        y = "Frequency"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- DAILY TREND ----------------
  
  output$daily_trend <- renderPlotly({
    daily <- selected_data() %>%
      group_by(DateOnly) %>%
      summarise(
        DailyMean = mean(Value),
        Records = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      daily,
      aes(
        DateOnly, DailyMean,
        text = paste(
          "Date:", DateOnly,
          "<br>Daily average:", round(DailyMean, 3),
          "<br>Valid records:", Records
        )
      )
    ) +
      geom_line(color = "#0072B2") +
      geom_point(color = "#D55E00", size = 1.5) +
      labs(
        x = "Date",
        y = paste("Daily average:", label_var(input$variable))
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$daily_table <- renderDT({
    selected_data() %>%
      group_by(DateOnly) %>%
      summarise(
        Average = mean(Value),
        Minimum = min(Value),
        Maximum = max(Value),
        ValidRecords = n(),
        .groups = "drop"
      ) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatRound(c("Average", "Minimum", "Maximum"), digits = 3)
  })
  
  # ---------------- MONTHLY AVERAGE ----------------
  
  output$monthly_average <- renderPlotly({
    md <- selected_data() %>%
      group_by(MonthDate) %>%
      summarise(Average = mean(Value), .groups = "drop")
    
    p <- ggplot(md, aes(MonthDate, Average)) +
      geom_col(fill = "#009E73") +
      labs(
        x = "Month",
        y = paste("Average", label_var(input$variable))
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- MONTHLY BOXPLOT ----------------
  
  output$monthly_boxplot <- renderPlotly({
    sd <- selected_data() %>%
      mutate(Month = format(MonthDate, "%Y-%m"))
    
    p <- ggplot(sd, aes(Month, Value)) +
      geom_boxplot(fill = "#56B4E9", outlier.alpha = 0.3) +
      labs(
        x = "Month",
        y = label_var(input$variable)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # ---------------- MONTHLY STATISTICS ----------------
  
  output$monthly_table <- renderDT({
    selected_data() %>%
      mutate(Month = format(MonthDate, "%Y-%m")) %>%
      group_by(Month) %>%
      summarise(
        Average = mean(Value),
        Median = median(Value),
        SD = if (n() > 1) sd(Value) else NA_real_,
        Minimum = min(Value),
        Maximum = max(Value),
        Records = n(),
        .groups = "drop"
      ) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatRound(
        c("Average", "Median", "SD", "Minimum", "Maximum"),
        digits = 3
      )
  })
  
  # ---------------- SCATTER PLOT ----------------
  
  output$scatter_plot <- renderPlotly({
    req(input$variable, input$variable2)
    
    validate(need(
      input$variable != input$variable2,
      "Choose two different variables in the sidebar."
    ))
    
    sd <- filtered_data() %>%
      transmute(
        X = .data[[input$variable]],
        Y = .data[[input$variable2]]
      ) %>%
      filter(is.finite(X), is.finite(Y))
    
    validate(need(
      nrow(sd) > 1,
      "Not enough paired observations for these variables."
    ))
    
    p <- ggplot(sd, aes(X, Y)) +
      geom_point(alpha = 0.45, color = "#0072B2") +
      geom_smooth(method = "lm", se = TRUE, color = "#D55E00") +
      labs(
        x = label_var(input$variable),
        y = label_var(input$variable2)
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- CORRELATION HEATMAP ----------------
  
  output$correlation_plot <- renderPlot({
    fd <- filtered_data() %>%
      select(all_of(variables))
    
    # Remove variables with too few values or no variation
    keep <- sapply(fd, function(x) {
      sum(!is.na(x)) >= 3 &&
        is.finite(sd(x, na.rm = TRUE)) &&
        sd(x, na.rm = TRUE) > 0
    })
    
    fd <- fd[, keep, drop = FALSE]
    
    validate(need(
      ncol(fd) >= 2,
      "At least two numeric variables with variation are required."
    ))
    
    correlation_matrix <- cor(
      fd,
      use = "pairwise.complete.obs"
    )
    
    corrplot(
      correlation_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.6,
      col = colorRampPalette(
        c("#2166AC", "white", "#B2182B")
      )(200)
    )
  })
  
  # ---------------- DATA TABLE ----------------
  
  output$data_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(Timestamp, DateOnly, all_of(variables)),
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # ---------------- DOWNLOAD DATA ----------------
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("cleaned_air_quality_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}

# ============================================================
# 9. START THE APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
