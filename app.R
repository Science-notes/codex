library(shiny)
library(rmarkdown)

prepare_workdir <- function(script_path, data_files) {
  workdir <- tempfile("cnps_cycle_")
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)

  file.copy(script_path, file.path(workdir, basename(script_path)))

  if (!is.null(data_files) && nrow(data_files) > 0) {
    file.copy(data_files$datapath, file.path(workdir, data_files$name))
  }

  workdir
}

render_cnps_script <- function(workdir, script_name, output_name) {
  output_file <- paste0(output_name, ".html")
  rmarkdown::render(
    input = file.path(workdir, script_name),
    output_file = output_file,
    output_dir = workdir,
    envir = new.env(parent = globalenv())
  )
  file.path(workdir, output_file)
}

ui <- fluidPage(
  titlePanel("CNPS.cycle Automated Execution"),
  sidebarLayout(
    sidebarPanel(
      fileInput("script", "Upload SampleData_AutomatedExecutionScript.Rmd", accept = c(".Rmd")),
      fileInput(
        "data_files",
        "Upload sample data files",
        multiple = TRUE
      ),
      textInput("output_name", "Output HTML name", value = "CNPS_cycle_report"),
      actionButton("run", "Run script", class = "btn-primary"),
      helpText("Tip: use the release assets from CNPS.cycle v1.0.0.")
    ),
    mainPanel(
      h4("Uploaded files"),
      tableOutput("uploads"),
      h4("Run status"),
      verbatimTextOutput("status"),
      downloadButton("download_report", "Download report")
    )
  )
)

server <- function(input, output, session) {
  output$uploads <- renderTable({
    req(input$script)
    files <- rbind(
      data.frame(
        type = "Rmd",
        name = input$script$name,
        stringsAsFactors = FALSE
      ),
      if (!is.null(input$data_files)) {
        data.frame(
          type = "Data",
          name = input$data_files$name,
          stringsAsFactors = FALSE
        )
      }
    )
    files
  })

  result <- eventReactive(input$run, {
    req(input$script)
    workdir <- prepare_workdir(input$script$datapath, input$data_files)
    script_name <- basename(input$script$datapath)
    output_name <- ifelse(nzchar(input$output_name), input$output_name, "CNPS_cycle_report")

    tryCatch(
      {
        output_path <- render_cnps_script(workdir, script_name, output_name)
        list(success = TRUE, output_path = output_path, workdir = workdir)
      },
      error = function(err) {
        list(success = FALSE, message = conditionMessage(err), workdir = workdir)
      }
    )
  })

  output$status <- renderText({
    req(result())
    if (result()$success) {
      paste(
        "Render completed.",
        paste("Output:", basename(result()$output_path)),
        sep = "\n"
      )
    } else {
      paste("Render failed:", result()$message)
    }
  })

  output$download_report <- downloadHandler(
    filename = function() {
      req(result())
      if (result()$success) basename(result()$output_path) else "CNPS_cycle_report.html"
    },
    content = function(file) {
      req(result())
      validate(need(result()$success, "No report to download."))
      file.copy(result()$output_path, file)
    }
  )
}

shinyApp(ui = ui, server = server)
