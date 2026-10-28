#' Upload module UI
#'
#' Sidebar inputs for choosing a delimited file, the QMRA wiki workbook, or the
#' bundled examples, with an optional column-mapping panel.
#'
#' @param id Module id.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  auto <- c("Auto-detect" = "")
  tagList(
    fileInput(ns("file"), "Dose-response file or QMRA wiki workbook", accept = c(".csv", ".tsv", ".txt", ".xlsx")),
    p(
      class = "text-muted small",
      "A delimited file is one experiment and goes straight to Fit. An .xlsx workbook is browsed on the Library tab,",
      "where you choose an experiment to send here."
    ),
    actionButton(ns("example"), "Use Ward rotavirus example data"),
    p(class = "text-muted small mt-1", "Ward et al. (1986) human rotavirus challenge study."),
    actionButton(ns("sample_workbook"), "Use QMRA wiki sample workbook"),
    p(
      class = "text-muted small mt-1",
      "14 experiments cut down from the QMRA wiki workbook, for demos and testing. Data compiled from the ",
      a("QMRA wiki", href = "https://qmrawiki.org", target = "_blank"),
      "; see each row's reference."
    ),
    bslib::accordion(
      id = ns("mapping_panel"),
      open = FALSE,
      class = "mt-3",
      bslib::accordion_panel(
        "Column mapping (optional)",
        p(class = "text-muted small", "Only needed when a delimited file's column names are not recognized automatically."),
        selectInput(ns("dose"), "Dose column", choices = auto),
        selectInput(ns("positive"), "Positive-response column", choices = auto),
        selectInput(ns("negative"), "Negative-response column", choices = auto)
      )
    )
  )
}

#' Upload module table UI
#'
#' Main-panel output showing the loaded workbook, then the standardized data
#' or the import error.
#'
#' @param id Module id.
#' @noRd
mod_upload_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("workbook_status")),
    uiOutput(ns("error")),
    uiOutput(ns("origin")),
    uiOutput(ns("summary")),
    tableOutput(ns("table"))
  )
}

#' Upload module server
#'
#' Holds two independent things: the current dataset, which Fit analyses, and
#' the QMRA wiki workbook, which the Library tab explores. A delimited file is
#' read with [singlehit::read_dose_response()] and a data frame sent from the
#' Library tab with [singlehit::as_dose_response()]; both standardize and
#' validate the grouped counts. An `.xlsx` file is read as a workbook with
#' [read_qmra_workbook()] and leaves the current dataset alone.
#'
#' @param id Module id.
#' @param incoming Reactive returning `NULL` or a list with `data` (a data frame
#'   of dose groups), `label` (where it came from), and `sent` (a counter, so
#'   sending the same experiment again reloads it).
#' @return A list of reactives: `data`, the standardized data or `NULL`;
#'   `workbook`, the workbook rows or `NULL`; `workbook_name`, where it came
#'   from; and `workbook_loaded`, a counter
#'   that changes each time a workbook loads successfully.
#' @noRd
mod_upload_server <- function(id, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    fields <- c("dose", "positive", "negative")
    no_mapping <- list(dose = NULL, positive = NULL, negative = NULL)
    # list(path, label) for a file, or list(data, label) sent from the Library tab.
    loaded <- reactiveVal(NULL)
    mapping <- reactiveVal(no_mapping)
    # A failed workbook read keeps the previous workbook and reports the error.
    workbook <- reactiveVal(NULL)
    workbook_name <- reactiveVal(NULL)
    workbook_error <- reactiveVal(NULL)
    workbook_loaded <- reactiveVal(0L)

    load_workbook <- function(path, name) {
      rows <- tryCatch(read_qmra_workbook(path), error = function(e) e)
      if (inherits(rows, "error")) {
        workbook_error(sprintf("Could not read %s: %s", name, conditionMessage(rows)))
        return(invisible())
      }
      workbook_error(NULL)
      workbook(rows)
      workbook_name(name)
      workbook_loaded(workbook_loaded() + 1L)
    }

    # A new source starts from auto-detection. The mapping is reset here, in the
    # same step as the source, so the new data are never parsed with the
    # previous source's column choices.
    load_source <- function(value) {
      # A failed workbook read is stale once something else loads.
      workbook_error(NULL)
      mapping(no_mapping)
      for (field in fields) {
        updateSelectInput(session, field, selected = "")
      }
      loaded(value)
    }

    observeEvent(input$file, {
      name <- input$file$name
      if (is.null(name)) name <- basename(input$file$datapath)
      if (tolower(tools::file_ext(name)) == "xlsx") {
        load_workbook(input$file$datapath, name)
      } else {
        load_source(list(path = input$file$datapath, label = name))
      }
    })
    observeEvent(
      input$example,
      load_source(list(
        path = system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"),
        label = "Ward rotavirus example data"
      ))
    )
    observeEvent(
      input$sample_workbook,
      load_workbook(app_sys("extdata", "qmra-wiki-sample.xlsx"), "the QMRA wiki sample workbook")
    )
    observeEvent(incoming(), {
      sent <- incoming()
      load_source(list(data = sent$data, label = paste(sent$label, "from the Library tab")))
    })

    # Same reader settings as singlehit::read_dose_response(), so the offered
    # names match what the real read sees.
    columns <- reactive({
      req(loaded())
      if (!is.null(loaded()$data)) {
        return(names(loaded()$data))
      }
      tryCatch(
        names(readr::read_delim(loaded()$path, n_max = 0L, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE)),
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
      read <- if (!is.null(loaded()$data)) {
        function() singlehit::as_dose_response(loaded()$data, chosen$dose, chosen$positive, chosen$negative)
      } else {
        function() singlehit::read_dose_response(loaded()$path, dose = chosen$dose, positive = chosen$positive, negative = chosen$negative)
      }
      tryCatch(read(), error = function(e) e)
    })

    output$origin <- renderUI({
      req(loaded())
      h6(class = "mt-2", "Current dataset: ", loaded()$label)
    })

    output$workbook_status <- renderUI({
      tagList(
        if (!is.null(workbook_error())) div(class = "alert alert-danger", workbook_error()),
        if (!is.null(workbook())) {
          div(
            class = "alert alert-info py-2",
            sprintf(
              "Workbook loaded: %d experiments from %s. Explore them and pick one to analyse on the Library tab.",
              length(unique(workbook()$experiment_key)), workbook_name()
            )
          )
        }
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

    list(
      data = reactive(if (inherits(parsed(), "error")) NULL else parsed()),
      workbook = reactive(workbook()),
      workbook_name = reactive(workbook_name()),
      workbook_loaded = reactive(workbook_loaded())
    )
  })
}
