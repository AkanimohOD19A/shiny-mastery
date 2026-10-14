library(shiny)
library(bs4Dash)
library(DT)
library(dplyr)
library(shinyWidgets)

#' @export
mod_queue_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "queue-toolbar",
        fluidRow(
          column(4, textInput(ns("search"), NULL,
                              placeholder = "Search merchant / case id")),
          column(3, selectInput(ns("tier"), "Tier",
                                c("all", "critical", "high", "medium", "low"),
                                selected = "all")),
          column(3, selectInput(ns("sla"), "SLA",
                                c("all", "breached", "at_risk", "ok"),
                                selected = "all")),
          column(2, actionButton(ns("refresh"), "Refresh",
                                 class = "btn-outline-primary w-100"))
        )
    ),
    DTOutput(ns("table"))
  )
}

#' @export
mod_queue_server <- function(id, cases_rv, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Debounce free-text search (400ms per spec). Wrap in reactive() because
    # debounce() must receive a reactive expression, not a value.
    search_d <- reactive(input$search %||% "") |> debounce(400)
    tier_d   <- reactive(input$tier %||% "all")
    sla_d    <- reactive(input$sla  %||% "all")
    
    filtered <- reactive({
      df <- cases_rv()
      s  <- search_d()
      if (!is.null(s) && nzchar(s)) {
        df <- df[grepl(s, df$merchant_name, ignore.case = TRUE) |
                   grepl(s, df$case_id,       ignore.case = TRUE), , drop = FALSE]
      }
      if (tier_d() != "all") df <- df[df$risk_tier == tier_d(), , drop = FALSE]
      if (sla_d()  != "all") df <- df[df$sla_state == sla_d(), , drop = FALSE]
      
      # Priority sort: critical first, then breached, then soonest-due
      df$tier_rank <- match(df$risk_tier,
                            c("critical", "high", "medium", "low"))
      df <- df[order(df$tier_rank,
                     df$sla_state != "breached",
                     df$due_at), , drop = FALSE]
      df$tier_rank <- NULL
      df
    })
    
    output$table <- renderDT({
      df <- filtered()
      
      # Columns shown in the table. Keep order aligned with `gaze`:
      # id -> merchant -> tier -> score -> status -> sla -> due -> owner
      cols <- c("case_id", "merchant_name", "risk_tier", "risk_score",
                "status", "sla_state", "due_at", "assigned_to")
      df_display <- df[, cols, drop = FALSE]
      
      # Format due_at for display; keep POSIXct for styleInterval via a
      # hidden numeric column would be cleaner, but for now format only.
      df_display$due_at <- format(df_display$due_at, "%Y-%m-%d %H:%M:%S")
      
      dt <- datatable(
        df_display,
        selection = "single",
        rownames  = FALSE,
        class     = "compact stripe hover",
        options   = list(
          pageLength = 15,
          order      = list(),
          columnDefs = list(
            list(className = "dt-center", targets = c(2, 3, 4, 5))
          ),
          # Progressive disclosure: click a row to expand a compact
          # child row with detail. Inline HTML avoids needing a
          # globally-defined format() function.
          initComplete = JS("
            function(settings, json) {
              var table = this.api();
              table.on('click', 'tbody tr', function () {
                var tr = $(this);
                var row = table.row(tr);
                if (row.child.isShown()) {
                  row.child.hide();
                  tr.removeClass('shown');
                } else {
                  var d = row.data();
                  var html =
                    '<div style=\"padding:8px 12px;background:#fafafa;' +
                    'border-left:3px solid #666;font-size:12px;\">' +
                    '<strong>Case:</strong> ' + d[0] + ' &nbsp; ' +
                    '<strong>Tier:</strong> ' + d[2] + ' &nbsp; ' +
                    '<strong>Score:</strong> ' + d[3] + ' &nbsp; ' +
                    '<strong>Due:</strong> ' + d[6] + ' &nbsp; ' +
                    '<strong>Owner:</strong> ' + (d[7] || '-') +
                    '</div>';
                  row.child(html).show();
                  tr.addClass('shown');
                }
              });
            }
          ")
        )
      ) |>
        formatStyle("risk_tier",
                    backgroundColor = styleEqual(
                      c("critical", "high", "medium", "low"),
                      c("#7f1d1d", "#b45309", "#0e7490", "#166534")),
                    color = "white", fontWeight = "600") |>
        formatStyle("sla_state",
                    color = styleEqual(
                      c("breached", "at_risk", "ok"),
                      c("#b91c1c", "#b45309", "#166534")),
                    fontWeight = "700")
      
      dt
    })
    
    # Return selected case id as a reactive
    list(
      selected = reactive({
        row <- input$table_rows_selected
        if (length(row) == 0) return(NULL)
        filtered()$case_id[row]
      }),
      refresh = reactive(input$refresh)
    )
  })
}

# Local null-coalescing helper; compliance_rules.R also defines `%||%`,
# but this file must be self-contained.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}