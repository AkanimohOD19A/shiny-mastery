# R/mod_whatif.R
mod_whatif_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("What-If Scenario"),
    p(class = "text-muted",
      "Adjust parameters and click Recompute. Base metrics are unaffected."),
    sliderInput(ns("score_cutoff"), "Min FICO:",
                min = 550, max = 750, value = 620, step = 10),
    sliderInput(ns("max_ltv"), "Max LTV:",
                min = 0.5, max = 1.1, value = 0.95, step = 0.01),
    sliderInput(ns("max_dti"), "Max DTI:",
                min = 0.10, max = 0.55, value = 0.43, step = 0.01),
    actionButton(ns("recompute"), "Recompute Scenario",
                 class = "btn-primary", icon = icon("calculator")),
    hr(),
    tableOutput(ns("delta"))
  )
}

mod_whatif_server <- function(id, kpis) {
  moduleServer(id, function(input, output, session) {
    # Heavy compute runs only on button click ??? deliberately NOT reactive
    scenario <- eventReactive(input$recompute, {
      base <- kpis()
      # Proportion of loans above the FICO cutoff (approximate via normal CDF)
      keep_pct <- 1 - pnorm(input$score_cutoff, base$avg_fico, 50)
      scale <- keep_pct *
        (1 - pmax(0, input$max_ltv - 0.85) * 2) *
        (1 - pmax(0, input$max_dti - 0.35) * 2)
      
      data.frame(
        Metric  = c("Loans", "Balance", "30+ DPD", "Charge-off"),
        Base    = c(base$n_loans, base$total_balance,
                    base$n_dpd_30, base$n_charge_off),
        Scenario = round(c(base$n_loans, base$total_balance,
                           base$n_dpd_30, base$n_charge_off) * scale)
      )
    })
    
    output$delta <- renderTable({
      df <- scenario()
      df$Delta <- df$Scenario - df$Base
      df
    })
  })
}