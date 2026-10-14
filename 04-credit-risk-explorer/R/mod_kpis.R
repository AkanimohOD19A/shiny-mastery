library(bslib)

## ui
mod_kpis_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3,3,3,3),
    value_box(
      "Loans",
      textOutput(ns("n_loans")),
      showcase = bsicons::bs_icon("collection"), theme = "primary"),
    value_box(
      "Balance", 
      textOutput(ns("balance")),
      showcase = bsicons::bs_icon("cash-stack"), theme = "success"),
    value_box(
      "30+ DPD Rate",
      textOutput(ns("dpd30")),
      showcase = bsicons::bs_icon("exclamation-triangle"), theme = "warning"),
    value_box(
      "Charge-off Rate", 
      textOutput(ns("co")),
      showcase = bsicons::bs_icon("x-octagon"), theme = "danger")
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
      round(100 * kpis()$n_dpd_30 / (kpis()$n_loans * 12), 2), "%"))
    output$co      <- renderText(paste0(
      round(100 * kpis()$n_charge_off / (kpis()$n_loans * 12), 2), "%"))
  })
}