#' Pooling module UI
#'
#' Tests whether experiments chosen on the Library tab can share one
#' dose-response curve, with the checks and the session log that guard against
#' choosing groups after seeing the fits (#20).
#'
#' @param id Module id.
#' @noRd
mod_pooling_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 340,
      checkboxGroupInput(
        ns("models"),
        "Models",
        choices = c("Exponential" = "exponential", "Beta-Poisson" = "beta_poisson", "Exact beta-Poisson (very slow)" = "exact_beta_poisson"),
        selected = c("exponential", "beta_poisson")
      ),
      numericInput(ns("alpha"), "Significance level (alpha)", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
      textAreaInput(
        ns("rationale"),
        "Why do these experiments belong together?",
        rows = 4,
        placeholder = "Decide this before running: for example, same pathogen, host, and route, so one curve is plausible."
      ),
      p(
        class = "text-muted small",
        "Required. It is recorded with the result, so groups are justified before the test, not chosen from it."
      ),
      uiOutput(ns("run_button")),
      hr(),
      h6("Session log"),
      uiOutput(ns("log_status")),
      downloadButton(ns("download_log"), "Download log (CSV)", class = "btn-sm")
    ),
    uiOutput(ns("empty")),
    uiOutput(ns("selection_ui")),
    uiOutput(ns("results_ui"))
  )
}

#' Pooling module server
#'
#' @param id Module id.
#' @param rows Reactive returning the workbook rows, or `NULL`.
#' @param experiments Reactive returning the [library_experiments()] table.
#' @param pool_set Reactive returning `NULL` or a list of `keys` and `sent`.
#' @param workbook_name Reactive returning where the workbook came from.
#' @param workbook_loaded Reactive counter that changes on every workbook load,
#'   including reloading the same file, which leaves `rows()` unchanged.
#' @return Nothing useful; the log is kept in the session.
#' @noRd
mod_pooling_server <- function(id, rows, experiments, pool_set, workbook_name = reactive(NULL), workbook_loaded = reactive(0L)) {
  moduleServer(id, function(input, output, session) {
    keys <- reactiveVal(character())
    result <- reactiveVal(NULL)
    # The log lasts the whole session, across workbooks (each row records its
    # workbook), so reloading cannot erase the record of tests already run.
    log <- reactiveVal(NULL)
    # A rationale is written for one set of experiments. After the set or the
    # workbook changes, the old text no longer counts until it is edited.
    rationale_stale <- reactiveVal(FALSE)

    start_over <- function(new_keys) {
      keys(new_keys)
      result(NULL)
      rationale_stale(TRUE)
      updateTextAreaInput(session, "rationale", value = "")
    }
    # Ahead of the other observers, so nothing reads the new workbook with the
    # previous workbook's keys.
    observeEvent(workbook_loaded(), start_over(character()), priority = 1)
    observeEvent(pool_set(), start_over(pool_set()$keys), priority = 1)
    observeEvent(input$rationale, rationale_stale(FALSE), ignoreInit = TRUE)

    selection <- reactive({
      req(rows(), length(keys()) > 0L)
      pool_selection(rows(), experiments(), keys())
    })
    checks <- reactive(pool_check(selection()))
    rationale <- reactive({
      if (rationale_stale() || is.null(input$rationale)) "" else trimws(input$rationale)
    })
    ready <- reactive({
      !pool_blocked(checks()) && nzchar(rationale()) && length(input$models) > 0L &&
        is.numeric(input$alpha) && isTRUE(input$alpha > 0 && input$alpha < 1)
    })

    output$empty <- renderUI({
      if (is.null(rows())) {
        p(class = "text-muted", "Load the QMRA wiki workbook on the Data tab, then choose a pooling set on the Library tab.")
      } else if (length(keys()) == 0L) {
        p(class = "text-muted", "Choose experiments in the Pooling set on the Library tab and press Test poolability.")
      }
    })

    output$selection_ui <- renderUI({
      req(nrow(selection()) > 0L)
      tagList(
        h5("Experiments"),
        tableOutput(session$ns("selection_table")),
        h5("Checks"),
        p(
          class = "text-muted small",
          "Blocking checks stop the test; warnings are shown and logged. These rules are a first, loose cut and will be",
          "revised (see pool_rules in R/fct_pooling.R)."
        ),
        tableOutput(session$ns("checks_table"))
      )
    })

    output$selection_table <- renderTable({
      shown <- selection()
      data.frame(
        Experiment = shown$label,
        Outcome = shown$response_type,
        `Dose unit` = shown$dose_unit,
        Host = shown$host,
        Route = shown$route,
        Doses = shown$doses,
        Subjects = format(shown$subjects, big.mark = ","),
        check.names = FALSE
      )
    })

    output$checks_table <- renderTable({
      shown <- checks()
      data.frame(
        Check = shown$label,
        Severity = ifelse(shown$severity == "block", "Blocks", "Warns"),
        Result = ifelse(shown$passed, "OK", ifelse(shown$severity == "block", "BLOCKED", "Warning")),
        Detail = shown$message,
        check.names = FALSE
      )
    })

    # Why Run is disabled, or "" when it is not. Held in a reactiveVal, which
    # ignores a set to the same value, so the button is redrawn only when its
    # state changes and not on every keystroke in the rationale.
    run_blocker <- reactiveVal("Choose a pooling set on the Library tab first.")
    observe(run_blocker(
      if (is.null(rows()) || length(keys()) == 0L) {
        "Choose a pooling set on the Library tab first."
      } else if (pool_blocked(checks())) {
        "A blocking check failed; see Checks."
      } else if (!nzchar(rationale())) {
        "Write why these experiments belong together first."
      } else if (length(input$models) == 0L) {
        "Choose at least one model."
      } else if (!isTRUE(ready())) {
        "Choose a significance level between 0 and 1."
      } else {
        ""
      }
    ))

    output$run_button <- renderUI({
      reason <- run_blocker()
      tagList(
        {
          button <- actionButton(session$ns("run"), "Run poolability test", icon = icon("play"), class = "btn-primary w-100")
          # Set the attribute directly: actionButton()'s own `disabled` argument
          # is recent. The server also refuses a run that is not ready.
          if (nzchar(reason)) tagAppendAttributes(button, disabled = NA) else button
        },
        if (nzchar(reason)) p(class = "text-muted small mt-1", reason)
      )
    })

    observeEvent(input$run, {
      req(isTRUE(ready()))
      chosen <- selection()
      run <- withProgress(message = "Fitting separate and pooled models", {
        tryCatch(
          pool_run(pool_datasets(rows(), chosen), models = input$models, alpha = input$alpha),
          error = function(e) e
        )
      })
      result(list(run = run, selection = chosen))
      if (!inherits(run, "error")) {
        entry <- pool_log_entry(run, chosen, checks(), rationale(), workbook_name())
        previous <- log()
        entry <- cbind(run = if (is.null(previous)) 1L else max(previous$run) + 1L, entry)
        log(rbind(previous, entry))
      }
    })

    output$log_status <- renderUI({
      entries <- log()
      tests <- if (is.null(entries)) 0L else length(unique(entries$run))
      p(
        class = "small",
        sprintf("%d poolability %s run this session.", tests, if (tests == 1L) "test" else "tests"),
        if (tests > 1L) " The p-values are not adjusted for running several tests."
      )
    })

    output$download_log <- downloadHandler(
      filename = function() paste0("singlehit-pooling-log-", format(Sys.Date()), ".csv"),
      content = function(file) {
        entries <- log()
        if (is.null(entries)) {
          entries <- data.frame(note = "No poolability tests have been run in this session.")
        }
        utils::write.csv(entries, file, row.names = FALSE)
      }
    )

    output$results_ui <- renderUI({
      current <- result()
      req(current)
      if (inherits(current$run, "error")) {
        return(div(class = "alert alert-danger mt-3", "The test could not run: ", conditionMessage(current$run)))
      }
      run <- current$run
      reference <- pool_wiki_reference(rows(), experiments(), current$selection)
      tagList(
        hr(),
        h5("Poolability test"),
        div(
          class = "alert alert-secondary small",
          "The likelihood-ratio test compares one pooled curve with a separate curve per experiment. With a few doses per",
          "experiment it has little power, so \"not rejected\" is not evidence that the experiments share one curve;",
          "read it together with the spread of the estimates below."
        ),
        if (length(run$warnings) > 0L) {
          div(class = "alert alert-warning small", strong("Fit warnings: "), paste(run$warnings, collapse = " "))
        },
        tableOutput(session$ns("test_table")),
        if (!is.null(reference)) {
          tagList(
            h6("The wiki's own pooled entry for these experiments"),
            tableOutput(session$ns("wiki_table"))
          )
        },
        h5("Separate and pooled fits"),
        plotOutput(session$ns("fit_plot"), height = "480px", fill = FALSE),
        h5("Spread of the estimates"),
        plotOutput(session$ns("forest_plot"), height = "380px", fill = FALSE)
      )
    })

    run_ok <- reactive({
      current <- result()
      req(current, !inherits(current$run, "error"))
      current$run
    })
    output$test_table <- renderTable(pool_test_table(run_ok()))
    output$wiki_table <- renderTable({
      current <- result()
      req(current)
      pool_wiki_reference(rows(), experiments(), current$selection)
    })
    output$fit_plot <- renderPlot(pool_plot(run_ok()), res = 96)
    output$forest_plot <- renderPlot(pool_forest(run_ok()), res = 96)

    invisible(NULL)
  })
}
