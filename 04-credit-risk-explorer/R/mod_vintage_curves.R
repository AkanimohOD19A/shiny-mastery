# R/mod_vintage_curves.R
library(plotly)

mod_vintage_curves_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("metric"), NULL,
                 choices = c("30+ DPD" = "dpd_30",
                             "90+ DPD" = "dpd_90",
                             "Charge-off" = "charge_off"),
                 inline = TRUE),
    plotlyOutput(ns("curves"), height = "420px")
  )
}

mod_vintage_curves_server <- function(id, con, filter) {
  moduleServer(id, function(input, output, session) {
    
    curves <- eventReactive(list(filter(), input$metric), {
      db_vintage_curves(con, filter(), input$metric)
    })
    
    output$curves <- renderPlotly({
      df <- curves()
      req(nrow(df) > 0)
      plot_ly(df,
              x = ~months_on_book, y = ~rate,
              color = ~vintage_label, type = "scatter", mode = "lines",
              hovertemplate = "MOB %{x}<br>Rate %{y:.2%}<extra></extra>") %>%
        layout(
          title = "Vintage Curves ??? Cumulative Rate by Months on Book",
          xaxis = list(title = "Months on Book"),
          yaxis = list(title = "Rate", tickformat = ".1%"),
          legend = list(title = list(text = "Vintage"))
        )
    })
  })
}