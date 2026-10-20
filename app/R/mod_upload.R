#' Upload module UI
#'
#' Sidebar inputs for choosing a delimited file or the bundled example, with an
#' optional column-mapping panel.
#'
#' @param id Module id.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  auto <- c("Auto-detect" = "")
  tagList(
    fileInput(ns("file"), "Dose-response file", accept = c(".csv", ".tsv", ".txt")),
    actionButton(ns("example"), "Use Ward rotavirus example data"),
    p(class = "text-muted small mt-1", "Ward et al. (1986) human rotavirus challenge study."),
    bslib::accordion(
      id = ns("mapping_panel"),
      open = FALSE,
      class = "mt-3",
      bslib::accordion_panel(
        "Column mapping (optional)",
        p(class = "text-muted small", "Only needed when column names are not recognized automatically."),
        selectInput(ns("dose"), "Dose column", choices = auto),
        selectInput(ns("positive"), "Positive-response column", choices = auto),
        selectInput(ns("negative"), "Negative-response column", choices = auto)
      )
    )
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
    uiOutput(ns("summary")),
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
    fields <- c("dose", "positive", "negative")
    no_mapping <- list(dose = NULL, positive = NULL, negative = NULL)
    source_path <- reactiveVal(NULL)
    mapping <- reactiveVal(no_mapping)

    # A new source starts from auto-detection. The mapping is reset here, in the
    # same step as the path, so the new file is never parsed with the previous
    # file's column choices.
    load_source <- function(path) {
      mapping(no_mapping)
      for (field in fields) {
        updateSelectInput(session, field, selected = "")
      }
      source_path(path)
    }

    observeEvent(input$file, load_source(input$file$datapath))
    observeEvent(
      input$example,
      load_source(system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"))
    )

    # Same reader settings as singlehit::read_dose_response(), so the offered
    # names match what the real read sees.
    columns <- reactive({
      req(source_path())
      tryCatch(
        names(readr::read_delim(source_path(), n_max = 0L, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE)),
        error = function(e) character()
      )
    })

    observeEvent(columns(), {
      named <- columns()
      names(named) <- named
      choices <- c("Auto-detect" = "", named)
      for (field in fields) {
        updateSelectInput(session, field, choices = choices, selected = "")
      }
    })

    observeEvent(list(input$dose, input$positive, input$negative), {
      pick <- function(value) if (is.null(value) || !nzchar(value)) NULL else value
      mapping(list(dose = pick(input$dose), positive = pick(input$positive), negative = pick(input$negative)))
    })

    parsed <- reactive({
      req(source_path())
      chosen <- mapping()
      tryCatch(
        singlehit::read_dose_response(
          source_path(),
          dose = chosen$dose,
          positive = chosen$positive,
          negative = chosen$negative
        ),
        error = function(e) e
      )
    })

    output$error <- renderUI({
      if (inherits(parsed(), "error")) {
        div(class = "alert alert-danger", conditionMessage(parsed()))
      }
    })
    output$summary <- renderUI({
      req(!inherits(parsed(), "error"))
      data <- parsed()
      p(class = "text-muted", sprintf("%d dose groups, %d subjects in total.", nrow(data), sum(data$total)))
    })
    output$table <- renderTable({
      req(!inherits(parsed(), "error"))
      data_preview(parsed())
    })

    reactive({
      if (inherits(parsed(), "error")) NULL else parsed()
    })
  })
}
