library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

# Read the CSV file
df_twd <- read.csv("df_twd.csv")

# Ensure the 'ts' column is of Date type
df_twd$ts <- as.Date(df_twd$ts)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tree Water Deficit in CH"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Dropdown menu for column selection (multiple selections allowed)
      selectInput("columns", "Select Columns to Display:",
                  choices = names(df_twd)[!(names(df_twd) %in% c("site_longitude", "site_latitude", "ts"))],  # Exclude longitude, latitude, and date columns
                  selected = "twd", multiple = TRUE),  # Default selection is "twd"
      
      # Slider for date selection
      sliderInput("date", "Select Date:",
                  min = min(df_twd$ts),
                  max = max(df_twd$ts),
                  value = min(df_twd$ts),
                  step = 1,
                  timeFormat = "%Y-%m-%d",  # Formatting the dates
                  animate = TRUE),
      
      # Slider for year selection
      sliderInput("year", "Select Year:",
                  min = min(year(df_twd$ts)),
                  max = max(year(df_twd$ts)),
                  value = min(year(df_twd$ts)),
                  animate = TRUE),
      
      # Slider for month selection
      sliderInput("month", "Select Month:",
                  min = 1, max = 12,
                  value = 1, animate = TRUE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Tabset panel for data and maps ----
      tabsetPanel(
        
        # Map tab based on Date ----
        tabPanel("Map by Date",
                 uiOutput("dynamic_maps_date")
        ),
        
        # Map tab based on Year and Month ----
        tabPanel("Map by Year and Month",
                 uiOutput("dynamic_maps_year_month")
        )
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset based on the selected date ----
  datasetInput <- reactive({
    df_twd %>%
      filter(ts == input$date)
  })
  
  # Return the dataset based on the selected year and month ----
  datasetYearMonthInput <- reactive({
    df_twd %>%
      filter(year(ts) == input$year, month(ts) == input$month)
  })
  
  # Generate a summary of the dataset ----
  output$caption <- renderText({
    input$caption
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = 10)
  })
  
  # Dynamically generate maps based on selected columns and Date ----
  output$dynamic_maps_date <- renderUI({
    req(input$columns)  # Ensure at least one column is selected
    
    map_outputs <- lapply(input$columns, function(col) {
      renderLeaflet({
        data <- datasetInput()
        
        # Create a color palette based on the selected column
        color_pal <- colorNumeric(palette = c("green", "yellow", "red"), domain = data[[col]])
        
        # Create the map with markers
        leaflet(data) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~site_longitude, lat = ~site_latitude,
            radius = 8, color = ~color_pal(data[[col]]),
            fill = TRUE, fillOpacity = 0.7,
            popup = ~paste(
              "Site: ", site, "<br>",
              "Species: ", species, "<br>",
              paste(col, ": ", round(data[[col]], 2)), "<br>",
              "Date: ", ts
            )
          ) %>%
          addLegend("bottomright", pal = color_pal, values = ~data[[col]], title = col, opacity = 1)
      })
    })
    
    # Return the list of dynamic maps
    do.call(tagList, map_outputs)
  })
  
  # Dynamically generate maps based on selected columns and Year/Month ----
  output$dynamic_maps_year_month <- renderUI({
    req(input$columns)  # Ensure at least one column is selected
    
    map_outputs <- lapply(input$columns, function(col) {
      renderLeaflet({
        data <- datasetYearMonthInput()
        
        # Create a color palette based on the selected column
        color_pal <- colorNumeric(palette = c("green", "yellow", "red"), domain = data[[col]])
        
        # Create the map with markers
        leaflet(data) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~site_longitude, lat = ~site_latitude,
            radius = 8, color = ~color_pal(data[[col]]),
            fill = TRUE, fillOpacity = 0.7,
            popup = ~paste(
              "Site: ", site, "<br>",
              "Species: ", species, "<br>",
              paste(col, ": ", round(data[[col]], 2)), "<br>",
              "Date: ", ts
            )
          ) %>%
          addLegend("bottomright", pal = color_pal, values = ~data[[col]], title = col, opacity = 1)
      })
    })
    
    # Return the list of dynamic maps
    do.call(tagList, map_outputs)
  })
}

# Create Shiny app ----
shinyApp(ui, server)
