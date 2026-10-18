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
  
  # 1. KPIs - NO WRAPPER NEEDED. The module handles the 4 columns.
  mod_kpis_ui("kpis"),
  
  br(),
  
  # 2. Vintage Curves Card (Added width=1 and fill=TRUE)
  card(
    width = 1,
    fill = TRUE,
    card_header("Vintage Performance"),
    mod_vintage_curves_ui("curves")
  ),
  
  br(),
  
  # 3. Loan Drill-Down Card (Added width=1 and fill=TRUE)
  card(
    width = 1,
    fill = TRUE,
    card_header("Loan Drill-Down"),
    DT::DTOutput("loan_sample")
  )
)

# server
server <- function(input, output, session) {
  con <- reactiveVal(db_connect())
  
  session$onSessionEnded(function(){
    if (!is.null(con())) dbDisconnect(con())
  })
  
  filters <- mod_filters_server("filters")
  
  filter <- reactive({
    req(filters$vintage(), filters$product(),
        filters$region(), filters$tier())
    build_filter_clause(filters$vintage(), filters$product(),
                        filters$region(),  filters$tier())
  })
  
  kpis <- reactive({ 
    req(filter())
    db_portfolio_kpis(con(), filter()) 
  })
  
  mod_kpis_server("kpis", kpis)
  mod_vintage_curves_server("curves", con(), filter)
  mod_whatif_server("whatif", kpis)
  
  # Changed to DT renderer so the table actually shows up!
  output$loan_sample <- DT::renderDataTable({
    req(filter())
    db_loan_sample(con(), filter(), limit = 200)
  })
}

# Link Both
shinyApp(ui, server)