# app.R
library(shiny)
library(bslib)

source("R/db.R")
source("R/mod_filters.R")
source("R/mod_kpis.R")
source("R/mod_vintage_curves.R")
source("R/mod_whatif.R")

# ui
ui <- page_sidebar(
  title = "Credit Risk & Vintage Analytics Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#1f3a5f"),
  
  sidebar = sidebar(
    width = 300,
    mod_filters_ui("filters", db_filter_choices(db_connect())),
    hr(),
    mod_whatif_ui("whatif")
  ),
  
  mod_kpis_ui("kpis"),
  br(),
  card(
    card_header("Vintage Performance"),
    mod_vintage_curves_ui("curves")
  ),
  br(),
  card(
    card_header("Loan Drill-Down"),
    tableOutput("loan_sample")
  )
)

# server
server <- function(input, output, session) {
  con <- reactiveVal(db_connect())
  session$onSessionEnded(function() dbDisconnect(con()))
  
  filters <- mod_filters_server("filters")
  
  # Build the WHERE clause + params once, share with everything downstream
  filter <- reactive({
    req(filters$vintage(), filters$product(),
        filters$region(), filters$tier())
    build_filter_clause(filters$vintage(), filters$product(),
                        filters$region(),  filters$tier())
  })
  
  kpis <- reactive({ db_portfolio_kpis(con(), filter()) })
  
  mod_kpis_server("kpis", kpis)
  mod_vintage_curves_server("curves", con(), filter)
  mod_whatif_server("whatif", kpis)
  
  output$loan_sample <- renderTable({
    db_loan_sample(con(), filter(), limit = 200)
  })
}

# Link Both
shinyApp(ui, server)