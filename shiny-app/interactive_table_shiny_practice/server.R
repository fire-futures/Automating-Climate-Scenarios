library(shiny)
library(rhandsontable)

server <- function(input, output, session) {
  # Store the table data in a reactiveValues object
  data <- reactiveValues(
    original_data = data.frame(
      notes = c("Likely segment", "Likely segment", "Likely segment"),
      min_t_l = c(0.01, 0.01, 0.01),
      min_t_u = c(0.99, 0.99, 0.99),
      max_t_l = c(0.01, 0.01, 0.01),
      max_t_u = c(0.99, 0.99, 0.99),
      precip_l = c(0.01, 0.01, 0.01),
      precip_u = c(0.99, 0.99, 0.99),
      wind_l = c(0.01, 0.01, 0.01),
      wind_u = c(0.99, 0.99, 0.99),
      min_h_l = c(0.01, 0.01, 0.01),
      min_h_u = c(0.99, 0.99, 0.99),
      max_h_l = c(0.01, 0.01, 0.01),
      max_h_u = c(0.99, 0.99, 0.99)
    ),
    modified_data = NULL
  )
  
  # Render the table using rhandsontable
  output$myTable <- renderRHandsontable({
    if (is.null(data$modified_data)) {
      rhandsontable(data$original_data, width = "100%", height = "300px")
    } else {
      rhandsontable(data$modified_data, width = "100%", height = "300px")
    }
  })
  
  # Use observeEvent to update the modified_data in the reactiveValues object whenever myTable changes
  observeEvent(input$myTable, {
    data$modified_data <- hot_to_r(input$myTable)
  })
  
  # Create a reactive expression for the climate_criteria
  climate_criteria <- reactive({
    data$modified_data
  })
  
  # Now you can use this reactive expression within your renderText functions
  output$test <- renderText({
    paste("Climate Criteria:",
          ifelse(is.null(climate_criteria()), "None", toString(climate_criteria())),
          "Class:",
          class(climate_criteria()))
  })
  
  output$math_check <- renderText({
    mean_precip_l = mean(climate_criteria()$precip_l, na.rm = TRUE)
    print(mean_precip_l)
  })
}