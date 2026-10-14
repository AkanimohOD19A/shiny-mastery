# app.R
library(dplyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bslib)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  # App title
  titlePanel("BMI Health Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Your Measurements"),
      
      # Height input with validation
      numericInput("height_cm", 
                   "Height (cm):", 
                   value = 175,
                   min = 50,
                   max = 250),
      
      # Weight input with validation
      numericInput("weight_kg", 
                   "Weight (kg):", 
                   value = 70,
                   min = 10,
                   max = 300),
      
      actionButton("calculate", 
                   "Calculate BMI", 
                   icon = icon("heart"),
                   class = "btn-primary btn-lg"),
      
      hr(),
      
      # Additional health metrics
      h4("Optional Details"),
      selectInput("activity_level",
                  "Activity Level:",
                  choices = c("Sedentary", 
                              "Lightly Active", 
                              "Moderately Active", 
                              "Very Active")),
      
      # This shows reactive UI in action
      conditionalPanel(
        condition = "input.activity_level == 'Very Active'",
        p("Great! You maintain high activity levels.",
          style = "color: green;")
      )
    ),
    
    mainPanel(
      # Value boxes for summary stats
      value_box(
        title = "Your BMI",
        value = textOutput("bmi_value"),
        showcase = bsicons::bs_icon("heart-pulse"),
        theme_color = "primary"
      ),
      
      value_box(
        title = "Category",
        value = textOutput("bmi_category"),
        showcase = bsicons::bs_icon("clipboard-data"),
        theme_color = "info"
      ),
      
      # More detailed output
      card(
        card_header("Health Assessment"),
        verbatimTextOutput("health_assessment")
      ),
      
      card(
        card_header("BMI Reference Table"),
        tableOutput("bmi_reference")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # This demonstrates req() and validate() in action
  bmi_data <- eventReactive(input$calculate, {
    # Validate inputs exist
    req(input$height_cm, input$weight_kg)
    
    # Validate reasonable ranges
    validate(
      need(input$height_cm > 50 && input$height_cm < 250, 
           "Height must be between 50 and 250 cm"),
      need(input$weight_kg > 10 && input$weight_kg < 300, 
           "Weight must be between 10 and 300 kg")
    )
    
    # Calculate BMI
    height_m <- input$height_cm / 100
    bmi <- input$weight_kg / (height_m ^ 2)
    
    # Determine category
    category <- case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal weight",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"
    )
    
    # Health recommendation
    recommendation <- case_when(
      bmi < 18.5 ~ "Consider gaining weight through healthy nutrition",
      bmi < 25 ~ "Maintain your healthy weight through balanced diet and exercise",
      bmi < 30 ~ "Consider weight loss strategies with your healthcare provider",
      TRUE ~ "Consult a healthcare provider for weight management support"
    )
    
    list(
      bmi = bmi,
      category = category,
      recommendation = recommendation,
      ideal_weight_range = paste0(
        round(18.5 * (height_m ^ 2), 1), 
        " - ", 
        round(24.9 * (height_m ^ 2), 1), 
        " kg"
      )
    )
  })
  
  # Output: BMI value (formatted)
  output$bmi_value <- renderText({
    bmi_data()$bmi %>%
      round(1) %>%
      paste("kg/m??")
  })
  
  # Output: BMI category with color coding
  output$bmi_category <- renderText({
    bmi_data()$category
  })
  
  # Output: Full assessment
  output$health_assessment <- renderPrint({
    data <- bmi_data()
    cat("BMI:", round(data$bmi, 1), "kg/m??\n")
    cat("Category:", data$category, "\n")
    cat("Ideal weight range:", data$ideal_weight_range, "\n")
    cat("\nRecommendation:\n", data$recommendation)
  })
  
  # Output: Reference table
  output$bmi_reference <- renderTable({
    data.frame(
      Category = c("Underweight", "Normal", "Overweight", "Obese"),
      BMI_Range = c("< 18.5", "18.5 - 24.9", "25 - 29.9", ">= 30"),
      Risk = c("Increased", "Least", "Increased", "High"),
      stringsAsFactors = FALSE
    )
  })
}

# Run the app
shinyApp(ui, server)