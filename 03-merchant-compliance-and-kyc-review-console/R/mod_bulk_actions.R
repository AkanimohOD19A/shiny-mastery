library(shiny)
library(bs4Dash)

#' @export
mod_bulk_actions_ui <- function(id) {
  ns <- NS(id)
  div(class = "bulk-bar",
      span(textOutput(ns("count"), inline = TRUE)),
      actionButton(ns("assign"),  "Assign to me", class = "btn-sm btn-outline-primary"),
      actionButton(ns("defer"),   "Defer 7d",     class = "btn-sm btn-outline-secondary"),
      actionButton(ns("reqinfo"), "Request info", class = "btn-sm btn-outline-info")
  )
}

#' @export
mod_bulk_actions_server <- function(id, selected_ids, on_action) {
  moduleServer(id, function(input, output, session) {
    output$count <- renderText({
      sprintf("%d selected", length(selected_ids()))
    })
    observeEvent(input$assign,  on_action(selected_ids(), "assign"))
    observeEvent(input$defer,   on_action(selected_ids(), "defer"))
    observeEvent(input$reqinfo, on_action(selected_ids(), "request_info"))
  })
}