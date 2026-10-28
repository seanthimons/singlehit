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
    uiOutput(ns("origin")),
    uiOutput(ns("summary")),
    tableOutput(ns("table"))
  )
}

#' Upload module server
#'
#' Reads the chosen file with [singlehit::read_dose_response()], or a data
#' frame sent from the Library tab with [singlehit::as_dose_response()]; both
#' standardize and validate the grouped counts.
#'
#' @param id Module id.
#' @param incoming Reactive returning `NULL` or a list with `data` (a data frame
#'   of dose groups), `label` (where it came from), and `sent` (a counter, so
#'   sending the same experiment again reloads it).
#' @return A reactive returning the standardized data, or `NULL` when nothing
#'   valid has been loaded.
#' @noRd
mod_upload_server <- function(id, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    fields <- c("dose", "positive", "negative")
    no_mapping <- list(dose = NULL, positive = NULL, negative = NULL)
    # A path to read, or a list(data, label) sent from the Library tab.
    loaded <- reactiveVal(NULL)
    mapping <- reactiveVal(no_mapping)

    # A new source starts from auto-detection. The mapping is reset here, in the
    # same step as the source, so the new data are never parsed with the
    # previous source's column choices.
    load_source <- function(value) {
      mapping(no_mapping)
      for (field in fields) {
        updateSelectInput(session, field, selected = "")
      }
      loaded(value)
    }

    observeEvent(input$file, load_source(input$file$datapath))
    observeEvent(
      input$example,
      load_source(system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"))
    )
    observeEvent(incoming(), {
      sent <- incoming()
      load_source(list(data = sent$data, label = sent$label))
    })

    # Same reader settings as singlehit::read_dose_response(), so the offered
    # names match what the real read sees.
    columns <- reactive({
      req(loaded())
      if (is.list(loaded())) {
        return(names(loaded()$data))
      }
      tryCatch(
        names(readr::read_delim(loaded(), n_max = 0L, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE)),
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
      req(loaded())
      chosen <- mapping()
      read <- if (is.list(loaded())) {
        function() singlehit::as_dose_response(loaded()$data, chosen$dose, chosen$positive, chosen$negative)
      } else {
        function() singlehit::read_dose_response(loaded(), dose = chosen$dose, positive = chosen$positive, negative = chosen$negative)
      }
      tryCatch(read(), error = function(e) e)
    })

    output$origin <- renderUI({
      if (is.list(loaded())) {
        p(class = "text-muted", "From the Library tab: ", strong(loaded()$label))
      }
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
