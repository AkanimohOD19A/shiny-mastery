library(bslib)

## ui
mod_kpis_ui <- function(id) {
  ns <- NS(id)
  
  # Use a slightly larger gap and let it fill the row
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    gap = "1rem",
    fill = TRUE, # Ensures they stretch to fill the space
    fillable = TRUE,
    value_box(
      "Loans",
      textOutput(ns("n_loans")),
      showcase = bsicons::bs_icon("collection"), 
      theme = "primary",
      min_height = "140px" # Gives the cards breathing room
    ),
    value_box(
      "Balance", 
      textOutput(ns("balance")),
      showcase = bsicons::bs_icon("cash-stack"), 
      theme = "success",
      min_height = "140px"
    ),
    value_box(
      "30+ DPD Rate",
      textOutput(ns("dpd30")),
      showcase = bsicons::bs_icon("exclamation-triangle"), 
      theme = "warning",
      min_height = "140px"
    ),
    value_box(
      "Charge-off Rate", 
      textOutput(ns("co")),
      showcase = bsicons::bs_icon("x-octagon"), 
      theme = "danger",
      min_height = "140px"
    )
  )
}


## server
mod_kpis_server <- function(id, kpis) {
  moduleServer(id, function(input, output, session) {
    output$n_loans <- renderText(format(kpis()$n_loans, big.mark = ","))
    output$balance <- renderText(paste0("$",
                                        format(round(kpis()$total_balance / 1e6, 1), 
                                               big.mark = ","), "M"))
    output$dpd30   <- renderText(paste0(
      round(100 * kpis()$n_dpd_30 / kpis()$n_loan_months, 2), "%"))
    output$co      <- renderText(paste0(
      round(100 * kpis()$n_charge_off / kpis()$n_loan_months, 2), "%"))
  })
}