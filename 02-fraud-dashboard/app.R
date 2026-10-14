library(shiny)
library(DT)
library(dplyr)
library(future)
library(promises)
library(lubridate)

plan(multisession, workers = 2)##max(1, parallel::detectCores() - 1))

# Verify it works; fall back to sequential if not
tryCatch(
  {
    f <- future({ Sys.getpid() })
    value(f)   # forces evaluation
    message("future: multisession OK, workers = 2")
  },
  error = function(e) {
    warning("future multisession failed, falling back to sequential: ",
            conditionMessage(e))
    plan(sequential)
  }
)

source("R/mod_risk_scoring.R")
source("R/mod_alert_thresholds.R")
source("R/mod_triage_table.R")
source("R/risk_scoring.R")   # pure functions used by tests + modules

ui <- fluidPage(
  titlePanel("Real-Time Transaction Fraud Monitoring"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      mod_alert_thresholds_ui("thresholds")
    ),
    mainPanel(
      width = 9,
      mod_risk_scoring_ui("scoring"),
      hr(),
      mod_triage_table_ui("triage")
    )
  )
)

server <- function(input, output, session) {
  # Simulated live stream ??? replace with real feed
  transactions <- reactivePoll(
    intervalMillis = 2000,
    session = session,
    checkFunc = function() Sys.time(),
    valueFunc = function() simulate_transactions(n = 5)
  )
  
  thresholds <- mod_alert_thresholds_server("thresholds")
  scored     <- mod_risk_scoring_server("scoring", transactions)
  triage     <- mod_triage_table_server("triage", scored, thresholds)
}

shinyApp(ui, server)