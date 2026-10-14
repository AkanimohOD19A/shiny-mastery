library(shiny)
library(future)
library(promises)
library(bs4Dash)
library(bslib)

#' @export
mod_sanctions_ui <- function(id) {
  ns <- NS(id)
  bs4Dash::box(
    bs4Dash::box("Sanctions / PEP Screening"),
    actionButton(ns("run"), "Re-screen (async)",
                 class = "btn-primary"),
    uiOutput(ns("status")),
    DTOutput(ns("results"))
  )
}

#' Simulated slow external API. In production, swap for httr2 call.
slow_screen_api <- function(name, list_df) {
  Sys.sleep(2)                        # simulate network latency
  sanctions_match(name, list_df)
}

#' @export
mod_sanctions_server <- function(id, case_row, sanctions_list) {
  moduleServer(id, function(input, output, session) {
    results <- reactiveVal(NULL)
    status  <- reactiveVal("idle")
    
    observeEvent(input$run, {
      cr <- case_row(); req(cr)
      status("running")
      output$status <- renderUI(div(class = "text-info", "Screening..."))
      
      # future_promise: runs on multisession worker, returns a promise
      p <- future_promise({
        slow_screen_api(cr$merchant_name, sanctions_list)
      }, seed = TRUE)
      
      p %...>% (function(res) {
        results(res)
        status("done")
        output$status <- renderUI(
          div(class = "text-success",
              sprintf("Done - %d matches", nrow(res))))
      }) %...!% (function(err) {
        status("error")
        output$status <- renderUI(
          div(class = "text-danger",
              paste("Screening failed:", conditionMessage(err))))
      })
    })
    
    output$results <- renderDT({
      r <- results()
      if (is.null(r) || nrow(r) == 0) {
        return(datatable(data.frame(Message = "No matches"),
                         rownames = FALSE))
      }
      datatable(r, rownames = FALSE)
    })
  })
}