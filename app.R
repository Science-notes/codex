library(shiny)

compute_cnps_cycle <- function(data, params) {
  validate(
    need(!is.null(data) && nrow(data) > 0, "Please upload a dataset to run CNPS.cycle."),
    need(!is.null(params$cycles) && params$cycles > 0, "Cycles must be greater than 0.")
  )

  data.frame(
    rows = nrow(data),
    columns = ncol(data),
    cycles = params$cycles,
    method = params$method,
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
  titlePanel("CNPS.cycle Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Upload data (CSV)", accept = c(".csv")),
      numericInput("cycles", "Number of cycles", value = 10, min = 1, step = 1),
      selectInput(
        "method",
        "Method",
        choices = c("default" = "default", "robust" = "robust")
      ),
      actionButton("run", "Run CNPS.cycle", class = "btn-primary")
    ),
    mainPanel(
      h4("Run summary"),
      tableOutput("summary"),
      h4("Notes"),
      verbatimTextOutput("notes")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath, stringsAsFactors = FALSE)
  })

  params <- reactive({
    list(cycles = input$cycles, method = input$method)
  })

  result <- eventReactive(input$run, {
    compute_cnps_cycle(dataset(), params())
  })

  output$summary <- renderTable({
    req(result())
    result()
  })

  output$notes <- renderText({
    paste(
      "This Shiny app currently provides a scaffold for the CNPS.cycle workflow.",
      "Replace compute_cnps_cycle() with the repository's core algorithm once available.",
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)
