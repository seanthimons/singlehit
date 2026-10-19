#' Upload module UI
#'
#' Sidebar inputs for choosing a delimited file or the bundled example.
#'
#' @param id Module id.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Dose-response file", accept = c(".csv", ".tsv", ".txt")),
    actionButton(ns("example"), "Use example data")
  )
}

#' Upload module table UI
#'
#' Main-panel output showing the standardized data or the import error.
#'
#' @param id Module id.
#' @noRd
mod_upload_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("error")),
    tableOutput(ns("table"))
  )
}

#' Upload module server
#'
#' Reads the chosen file with [singlehit::read_dose_response()], which
#' standardizes and validates the grouped counts.
#'
#' @param id Module id.
#' @return A reactive returning the standardized data, or `NULL` when nothing
#'   valid has been loaded.
#' @noRd
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    source_path <- reactiveVal(NULL)

    observeEvent(input$file, source_path(input$file$datapath))
    observeEvent(
      input$example,
      source_path(system.file("extdata", "dose-response-example.csv", package = "singlehit"))
    )

    parsed <- reactive({
      req(source_path())
      tryCatch(
        singlehit::read_dose_response(source_path()),
        error = function(e) e
      )
    })

    output$error <- renderUI({
      if (inherits(parsed(), "error")) {
        div(class = "alert alert-danger", conditionMessage(parsed()))
      }
    })
    output$table <- renderTable({
      req(!inherits(parsed(), "error"))
      parsed()
    })

    reactive({
      if (inherits(parsed(), "error")) NULL else parsed()
    })
  })
}
