library(shiny)
library(DT)
library(bslib)

#' @export
mod_audit_trail_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    bs4Dash::box("Audit Trail (append-only)"),
    DTOutput(ns("trail"))
  )
}

#' @export
mod_audit_trail_server <- function(id, trail_rv, selected_case = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$trail <- renderDT({
      df <- trail_rv()
      sel <- selected_case()
      if (!is.null(sel)) df <- df[df$case_id == sel, , drop = FALSE]
      df <- df[order(df$timestamp, decreasing = TRUE), , drop = FALSE]
      datatable(df, rownames = FALSE, options = list(pageLength = 20,
                                                     dom = "tip"))
    })
  })
}