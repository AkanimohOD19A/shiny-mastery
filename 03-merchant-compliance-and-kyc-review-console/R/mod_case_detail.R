# R/mod_case_detail.R
library(shiny)
library(bs4Dash)

#' @export
mod_case_detail_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "case-detail-wrap",
    uiOutput(ns("header")),
    tabBox(
      id = ns("detail_tabs"),
      width = 12,
      tabPanel("Documents",   tableOutput(ns("docs"))),
      tabPanel("Extracted",   tableOutput(ns("fields"))),
      tabPanel("Screening",   uiOutput(ns("screening"))),
      tabPanel("Txn Pattern", tableOutput(ns("txn")))
    )
  )
}

#' @export
mod_case_detail_server <- function(id, case_row) {
  moduleServer(id, function(input, output, session) {
    output$header <- renderUI({
      cr <- case_row()
      if (is.null(cr)) return(div(class = "text-muted p-3", "Select a case"))
      div(class = "case-header",
          h4(sprintf("%s - %s", cr$case_id, cr$merchant_name)),
          span(class = paste0("badge tier-", cr$risk_tier), cr$risk_tier),
          span(class = "ms-2 text-muted",
               sprintf("Risk %s | Due %s | %s",
                       cr$risk_score,
                       format(cr$due_at, "%Y-%m-%d %H:%M"),
                       cr$status))
      )
    })
    
    output$docs <- renderTable({
      req(case_row())
      data.frame(
        Document = c("Certificate of Incorporation","Director ID",
                     "Proof of Address","Bank Statement"),
        Status   = c("received","received","partial","missing"),
        Uploaded = c("2026-01-10","2026-01-10","2026-01-12","-"),
        stringsAsFactors = FALSE
      )
    })
    
    output$fields <- renderTable({
      cr <- case_row(); req(cr)
      data.frame(
        Field = c("Declared MCC","Observed MCC","Jurisdiction","Tenure (days)"),
        Value = c(cr$declared_mcc %||% "NA", cr$txn_mcc %||% "NA",
                  cr$jurisdiction_risk, cr$tenure_days),
        stringsAsFactors = FALSE
      )
    })
    
    output$screening <- renderUI({
      cr <- case_row(); req(cr)
      hits <- cr$sanctions_hits[[1]]
      if (is.null(hits) || nrow(hits) == 0) {
        return(div(class = "alert alert-success", "No sanctions / PEP hits"))
      }
      tagList(
        div(class = "alert alert-danger",
            sprintf("%d potential match(es)", nrow(hits))),
        tableOutput(session$ns("hits_table"))
      )
    })
    
    output$hits_table <- renderTable({
      cr <- case_row(); req(cr)
      cr$sanctions_hits[[1]]
    })
    
    output$txn <- renderTable({
      cr <- case_row(); req(cr)
      data.frame(
        Metric = c("30d volume","30d count","Avg ticket",
                   "Declared vs observed MCC match"),
        Value  = c("$482,000","1,204","$400",
                   ifelse(cr$declared_mcc == cr$txn_mcc, "yes", "NO")),
        stringsAsFactors = FALSE
      )
    })
  })
}