mod_filters_ui <- function(id, choices) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("vintage"), "Vintage:",
                   choices = choices$vintage, multiple = TRUE,
                   selected = choices$vintage),
    selectizeInput(ns("product"), "Product:",
                    choices = choices$product, multiple = TRUE,
                    selected = choices$product),
    selectizeInput(ns("region"), "Region:",
                   choices = choices$region, multiple = TRUE,
                   selected = choices$region),
    selectizeInput(ns("tier"), "Risk Tier:",
                    choices = choices$tier, multiple = TRUE,
                    selected = choices$tier),
  )
}

mod_filters_server <- function(id) {
  moduleServer(id, function(input, output, session){
    list(
      vintage = reactive({ input$vintage }),
      product = reactive({ input$product }),
      region  = reactive({ input$region  }),
      tier    = reactive({ input$tier    })
    )
  })
}