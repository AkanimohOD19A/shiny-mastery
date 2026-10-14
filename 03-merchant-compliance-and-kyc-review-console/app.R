library(shiny)
library(bs4Dash)
library(bslib)
library(future)
library(promises)
library(dplyr)
library(lubridate)
library(glue)

# Register www/ at a stable, session-independent URL
shiny::addResourcePath("kyc-assets", "www")

# --- Async plan with safe fallback (Windows-friendly) ---
tryCatch({
  plan(multisession, workers = 2)
  message("future: multisession plan active (2 workers)")
}, error = function(e) {
  message("future: multisession failed (", conditionMessage(e),
          "); falling back to sequential")
  plan(sequential)
})

# --- Load module files (self-contained, each does its own library()) ---
for (f in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}
source("R/compliance_rules.R", local = FALSE)

# --- Seed data (in production: load from DB / API) ---
source("inst/data/seed.R", local = TRUE)

# --- UI ---
ui <- bs4DashPage(
  title = "Merchant KYC Console",
  header = bs4DashNavbar(title = "KYC Console"),
  sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "kyc-assets/app.css")
      ##tags$script(src = "www/sla.js")
    ),
    fluidRow(
      column(
        width = 5,
        class = "queue-col",
        mod_queue_ui("queue")
      ),
      column(
        width = 7,
        class = "detail-col",
        mod_case_detail_ui("detail"),
        mod_action_panel_ui("actions"),
        mod_bulk_actions_ui("bulk")
      )
    ),
    fluidRow(
      column(width = 12, mod_sanctions_ui("sanctions"))
    ),
    fluidRow(
      column(width = 12, mod_audit_trail_ui("audit"))
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  current_user <- reactive("analyst@example.com")
  
  cases_rv <- reactiveVal(cases_seed)
  trail_rv <- reactiveVal(audit_trail_init())
  
  queue_out <- mod_queue_server("queue", cases_rv, current_user)
  selected_case_id <- queue_out$selected
  
  case_row <- reactive({
    id <- selected_case_id()
    if (is.null(id)) return(NULL)
    cases_rv()[cases_rv()$case_id == id, , drop = FALSE][1, ]
  })
  
  # Single source of truth for state transitions
  apply_transition <- function(case_id, action, note, user, decision = NULL) {
    df <- cases_rv()
    i  <- which(df$case_id == case_id)
    if (length(i) == 0) return(invisible(NULL))
    before <- df$status[i]
    after <- tryCatch(
      next_status(before, action),
      error = function(e) {
        shiny::showNotification(
          sprintf("Cannot %s a case that is '%s': %s",
                  action, before, conditionMessage(e)),
          type = "warning", duration = 5
        )
        before
      }
    )
    
    df$status[i] <- after
    if (action == "assign")       df$assigned_to[i] <- user
    if (action == "defer")        df$due_at[i] <- df$due_at[i] + lubridate::days(7)
    if (action == "decide")       df$decision[i] <- decision
    if (action == "request_info") df$status[i] <- "awaiting_merchant"
    if (action == "reopen")       df$decision[i] <- NA
    
    # Recompute SLA state each mutation
    df$sla_state <- vapply(seq_len(nrow(df)), function(k) {
      sla_state(sla_remaining_hours(Sys.time(), df$due_at[k]))
    }, character(1))
    
    cases_rv(df)
    
    trail_rv(audit_append(
      trail = trail_rv(), user = user, case_id = case_id,
      action = action, before_state = before, after_state = after,
      note = note
    ))
  }
  
  mod_case_detail_server("detail", case_row)
  
  mod_action_panel_server(
    "actions", case_row, current_user,
    on_decision = function(case_id, decision, note, user) {
      apply_transition(case_id, "decide", note, user, decision = decision)
    }
  )
  
  selected_ids <- reactive({
    id <- selected_case_id()
    if (is.null(id)) character(0) else id
  })
  
  mod_bulk_actions_server(
    "bulk", selected_ids,
    on_action = function(ids, action) {
      for (id in ids) apply_transition(id, action, "bulk", current_user())
    }
  )
  
  mod_sanctions_server("sanctions", case_row, sanctions_sample)
  mod_audit_trail_server("audit", trail_rv, selected_case = selected_case_id)
}

shinyApp(ui, server)
