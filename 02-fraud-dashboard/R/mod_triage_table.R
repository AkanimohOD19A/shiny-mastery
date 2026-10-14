mod_triage_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Analyst Triage Queue"),
    DTOutput(ns("triage_table")),
    actionButton(ns("mark_reviewed"), "Mark selected as reviewed",
                 class = "btn-primary")
  )
}

#' @param scored_transactions reactive returning scored data.frame
#' @param thresholds reactive returning threshold list
#' @return reactive data.frame of triage state (with reviewed flag)
mod_triage_table_server <- function(id, scored_transactions, thresholds) {
  moduleServer(id, function(input, output, session) {
    
    reviewed_ids <- reactiveVal(character())
    
    queued <- reactive({
      s <- scored_transactions()
      if (is.null(s) || isTRUE(s$error)) return(NULL)
      th <- thresholds()
      
      flagged <- apply_thresholds(s, th)
      flagged$reviewed <- flagged$id %in% reviewed_ids()
      flagged
    })
    
    output$triage_table <- renderDT({
      q <- queued()
      req(q)
      th <- thresholds()
      
      datatable(
        q,
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 25, order = list(list(3, "desc")))
      ) |>
        formatStyle(
          "risk_score",
          backgroundColor = styleInterval(
            c(th$risk_cutoff - 15, th$risk_cutoff),
            c("#f0f0f0", "#fff3cd", "#f8d7da")
          )
        )
    })
    
    observeEvent(input$mark_reviewed, {
      sel <- input$triage_table_rows_selected
      req(length(sel) == 1)
      q <- queued()
      reviewed_ids(c(reviewed_ids(), q$id[sel]))
    })
    
    return(queued)
  })
}