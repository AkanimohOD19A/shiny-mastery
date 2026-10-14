mod_alert_thresholds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Alert Thresholds"),
    sliderInput(ns("risk_cutoff"), "High-risk cutoff",
                min = 50, max = 100, value = 85, step = 1),
    sliderInput(ns("velocity_cutoff"), "Velocity (txns / 5 min)",
                min = 1, max = 20, value = 5, step = 1),
    checkboxInput(ns("flag_international"),
                  "Flag international transactions", value = TRUE)
  )
}

#' @return reactive list of threshold settings
mod_alert_thresholds_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        risk_cutoff        = input$risk_cutoff,
        velocity_cutoff    = input$velocity_cutoff,
        flag_international = input$flag_international
      )
    })
  })
}