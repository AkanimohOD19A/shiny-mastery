library(shiny)
library(bs4Dash)
library(bslib)

#' @export
mod_action_panel_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    title = "Decision",
    width = 12,
    status = "primary",
    solidHeader = FALSE,
    textAreaInput(ns("note"), "Justification (min 10 chars)",
                  rows = 4, width = "100%"),
    div(class = "d-flex gap-2 mt-2",
        actionButton(ns("approve"),  "Approve",  class = "btn-success flex-fill"),
        actionButton(ns("reject"),   "Reject",   class = "btn-danger flex-fill"),
        actionButton(ns("escalate"), "Escalate", class = "btn-warning flex-fill")
    ),
    uiOutput(ns("validation_msg"))
  )
}

#' @export
mod_action_panel_server <- function(id, case_row, current_user,
                                    on_decision) {
  moduleServer(id, function(input, output, session) {
    decision <- reactiveVal(NULL)
    
    observeEvent(input$approve,  decision("approve"))
    observeEvent(input$reject,   decision("reject"))
    observeEvent(input$escalate, decision("escalate"))
    
    observeEvent(decision(), {
      d <- decision(); req(d)
      cr <- case_row()
      v <- validate_decision(cr, d, input$note)
      if (!v$ok) {
        output$validation_msg <- renderUI({
          div(class = "alert alert-danger mt-2", v$reason)
        })
        decision(NULL)
        return()
      }
      ok <- on_decision(case_id = cr$case_id,
                        decision = d,
                        note = input$note,
                        user = current_user())
      if (isFALSE(ok)) {
        output$validation_msg <- renderUI({
          div(class = "alert alert-warning mt-2",
              "Not recorded: the case could not make that transition.")
        })
        decision(NULL)
        return()
      }
      output$validation_msg <- renderUI({
        div(class = "alert alert-success mt-2",
            sprintf("Recorded: %s", d))
      })
      updateTextAreaInput(session, "note", value = "")
      decision(NULL)
    })
  })
}