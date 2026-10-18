#' Risk Scoring Module - UI
#' @param id namespace
mod_risk_scoring_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Risk Scoring Engine"),
    uiOutput(ns("scoring_status")),
    tableOutput(ns("last_scores"))
  )
}

#' Risk Scoring Module - Server
#'
#' @param id namespace
#' @param transactions reactive returning a data.frame of new transactions
#' @return reactive list with scored results
mod_risk_scoring_server <- function(id, transactions) {
  moduleServer(id, function(input, output, session) {
    
    scored <- reactiveVal(NULL)
    
    observeEvent(transactions(), {
      req(nrow(transactions()) > 0)
      txns <- transactions()
      
      # Async: keep UI responsive while scoring runs in background
      future({
        score_transactions(txns)
      }) %...>% (function(result) {
        scored(result)
      }) %...!% (function(err) {
        # Fail gracefully - log and expose error state
        scored(list(error = TRUE, message = conditionMessage(err)))
      })
    })
    
    output$scoring_status <- renderUI({
      s <- scored()
      if (is.null(s)) {
        tags$em("Awaiting transactions...")
      } else if (isTRUE(s$error)) {
        tags$span(class = "text-danger", paste("Scoring error:", s$message))
      } else {
        tags$span(class = "text-success",
                  sprintf("Scored %d transactions at %s",
                          nrow(s), format(Sys.time(), "%H:%M:%S")))
      }
    })
    
    output$last_scores <- renderTable({
      s <- scored()
      if (is.null(s) || isTRUE(s$error)) return(NULL)
      head(s[, c("id", "amount", "risk_score", "velocity_flag")], 5)
    })
    
    return(reactive(scored()))
  })
}