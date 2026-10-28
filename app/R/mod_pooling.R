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
    uiOutput(ns("results_ui")),
    uiOutput(ns("explore_ui"))
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
#' @return A list with `sent`: `NULL` until pooled data are sent to the Data
#'   tab, then a list of `data`, `label`, `source`, `components` (each
#'   experiment's dose groups with its metadata, from [pool_components()]),
#'   and `sent` (a counter).
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
    # Where the current set came from, every set of experiments explored this
    # session, and the exploration of the current set.
    origin <- reactiveVal("Library")
    explored <- reactiveVal(list())
    exploration <- reactiveVal(NULL)
    sent <- reactiveVal(NULL)

    start_over <- function(new_keys, from = "Library") {
      keys(new_keys)
      origin(from)
      result(NULL)
      exploration(NULL)
      rationale_stale(TRUE)
      updateTextAreaInput(session, "rationale", value = "")
    }
    # Ahead of the other observers, so nothing reads the new workbook with the
    # previous workbook's keys.
    # Explored sets are kept across workbook loads, like the log: reloading
    # must not make a data-informed set look prespecified.
    observeEvent(workbook_loaded(), start_over(character()), priority = 1)
    observeEvent(pool_set(), start_over(pool_set()$keys), priority = 1)

    # A set chosen from an exploration, or lying within a set already explored
    # (however it was chosen), is data-informed: its test p-values are biased
    # toward pooling. Re-picking the same experiments on the Library tab does
    # not reset this.
    set_origin <- reactive({
      if (origin() != "Library") {
        origin()
      } else if (pool_data_informed(keys(), explored())) {
        "Library, within a set explored this session"
      } else {
        "Library"
      }
    })

    append_log <- function(entry) {
      previous <- log()
      entry <- cbind(run = if (is.null(previous)) 1L else max(previous$run) + 1L, entry)
      log(rbind(previous, entry))
    }
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
        if (set_origin() != "Library") {
          div(
            class = "alert alert-warning",
            strong("Data-informed set. "),
            sprintf("Source: %s. ", set_origin()),
            "These experiments were chosen after seeing poolability results for them, so the test below is biased",
            "toward pooling and its p-values are not confirmatory. Say so if you report a pooled fit."
          )
        },
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
        Strain = shown$strain,
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
        append_log(pool_log_entry(run, chosen, checks(), rationale(), workbook_name(), set_origin()))
      }
    })

    output$log_status <- renderUI({
      entries <- log()
      count <- function(kind) if (is.null(entries)) 0L else length(unique(entries$run[entries$kind == kind]))
      tests <- count("test")
      explorations <- count("exploration")
      # One exploration row per model, each with that model's pairwise tests.
      pairwise <- if (is.null(entries)) 0 else sum(entries$n_pairwise_tests[entries$kind == "exploration"])
      p(
        class = "small",
        sprintf(
          "%d poolability %s and %d %s run this session.",
          tests, if (tests == 1L) "test" else "tests",
          explorations, if (explorations == 1L) "exploration" else "explorations"
        ),
        if (explorations > 0L) sprintf(" The explorations made %d pairwise tests.", as.integer(pairwise)),
        if (tests > 1L || explorations > 0L) " None of the p-values are adjusted for running several tests."
      )
    })

    output$download_log <- downloadHandler(
      filename = function() paste0("singlehit-pooling-log-", format(Sys.Date()), ".csv"),
      content = function(file) {
        entries <- log()
        if (is.null(entries)) {
          entries <- data.frame(note = "No poolability tests or explorations have been run in this session.")
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
        plotOutput(session$ns("forest_plot"), height = "380px", fill = FALSE),
        h5("Analyse the pooled data"),
        if (all(!run$test$poolable)) {
          div(class = "alert alert-warning small", "The test rejected pooling under every model chosen.")
        },
        p(
          class = "text-muted small",
          "Sends the stacked experiments to the Data tab as one dataset for Fit, Compare, Bootstrap, and Plots. singlehit",
          "adds up counts at doses the experiments share: the fitted parameters are the same as the pooled fit here,",
          "but goodness of fit and the bootstrap use the merged dose groups, while the test above keeps each",
          "experiment's rows separate."
        ),
        actionButton(session$ns("send"), "Send pooled data to Data and Fit", icon = icon("arrow-right"), class = "btn-primary")
      )
    })

    send_count <- reactiveVal(0L)
    observeEvent(input$send, {
      current <- result()
      req(current, !inherits(current$run, "error"), identical(current$selection$experiment_key, selection()$experiment_key))
      chosen <- current$selection
      informed <- set_origin() != "Library"
      built <- tryCatch(
        {
          datasets <- pool_datasets(rows(), chosen)
          list(data = pool_fit_data(datasets, chosen, informed), components = pool_components(datasets, chosen))
        },
        error = function(e) e
      )
      data <- if (inherits(built, "error")) built else built$data
      if (inherits(data, "error")) {
        showNotification(conditionMessage(data), type = "error")
        return()
      }
      send_count(send_count() + 1L)
      label <- paste("Pooled:", paste(chosen$label, collapse = " + "))
      if (informed) {
        label <- paste0(label, " [data-informed set]")
      }
      sent(list(
        data = data,
        label = label,
        source = "Pooling tab",
        components = built$components,
        sent = send_count()
      ))
      append_log(pool_log_rows(
        kind = "sent to Fit",
        selection = chosen,
        models = NA_character_,
        alpha = current$run$alpha,
        workbook = workbook_name(),
        set_origin = set_origin()
      ))
    })

    # Exploration -----------------------------------------------------------

    output$explore_ui <- renderUI({
      req(nrow(selection()) > 0L)
      blocked <- pool_blocked(checks())
      too_many <- nrow(selection()) > pool_explore_limit
      button <- actionButton(session$ns("explore"), "Explore pairwise poolability", icon = icon("magnifying-glass"), class = "btn-outline-secondary")
      tagList(
        hr(),
        h5("Exploratory: which experiments pool with which"),
        div(
          class = "alert alert-warning small",
          strong("Not for choosing groups to report as pooled. "),
          "This searches the set for mutually poolable groups (singlehit's group_datasets(): every pairwise test, then",
          "a grouping). Groups found this way are chosen by the data, so a test of them is biased toward pooling (#20).",
          "Each exploration is logged, and a set taken from it is marked data-informed. The exact beta-Poisson model",
          sprintf("is left out, and at most %d experiments are searched.", pool_explore_limit)
        ),
        if (blocked || too_many) tagAppendAttributes(button, disabled = NA) else button,
        if (blocked) p(class = "text-muted small mt-1", "A blocking check failed; see Checks."),
        if (too_many) p(class = "text-muted small mt-1", sprintf("Choose at most %d experiments to explore.", pool_explore_limit)),
        uiOutput(session$ns("explore_results"))
      )
    })

    observeEvent(input$explore, {
      req(!pool_blocked(checks()), nrow(selection()) <= pool_explore_limit)
      chosen <- selection()
      models <- if (length(input$models) > 0L) input$models else c("exponential", "beta_poisson")
      explore <- withProgress(message = "Testing every pair and grouping", {
        tryCatch(pool_explore(pool_datasets(rows(), chosen), models, alpha = input$alpha), error = function(e) e)
      })
      exploration(list(explore = explore, selection = chosen))
      if (!inherits(explore, "error")) {
        # Log first: set_origin() describes the set as it was before this
        # exploration.
        append_log(pool_explore_log_entry(explore, chosen, workbook_name(), set_origin()))
        explored(c(explored(), list(chosen$experiment_key)))
      }
    })

    explore_ok <- reactive({
      current <- exploration()
      req(current, !inherits(current$explore, "error"))
      current
    })

    output$explore_results <- renderUI({
      current <- exploration()
      req(current)
      if (inherits(current$explore, "error")) {
        return(div(class = "alert alert-danger mt-3", "The exploration could not run: ", conditionMessage(current$explore)))
      }
      explore <- current$explore
      choices <- pool_group_choices(explore)
      tagList(
        p(
          class = "text-muted small mt-3",
          sprintf(
            "%d pairwise tests; groups by the %s method%s.",
            explore$n * (explore$n - 1L) / 2L,
            explore$method,
            if (explore$method == "exhaustive") sprintf(" over all %d ways to split the set", pool_bell(explore$n)) else " (greedy merges)"
          )
        ),
        if (length(explore$warnings) > 0L) {
          div(class = "alert alert-warning small", strong("Fit warnings: "), paste(explore$warnings, collapse = " "))
        },
        plotOutput(session$ns("explore_plot"), height = "420px", fill = FALSE),
        pool_heatmap_help(),
        tableOutput(session$ns("explore_groups")),
        if (length(choices) > 0L) {
          div(
            class = "d-flex flex-wrap align-items-end gap-3",
            selectInput(session$ns("group"), "Group to test", choices = choices, width = "560px"),
            actionButton(session$ns("use_group"), "Use this group as the pooling set", class = "btn-outline-warning mb-3")
          )
        } else {
          p(class = "text-muted", "No two experiments were grouped together.")
        }
      )
    })

    output$explore_plot <- renderPlot(pool_explore_plot(explore_ok()$explore), res = 96)
    output$explore_groups <- renderTable({
      explore <- explore_ok()$explore
      data.frame(
        Model = model_label_ui(explore$models),
        Groups = vapply(explore$models, function(model) pool_groups_text(explore, model), character(1L)),
        check.names = FALSE
      )
    })

    observeEvent(input$use_group, {
      current <- explore_ok()
      req(input$group %in% pool_group_choices(current$explore))
      model <- strsplit(input$group, "::", fixed = TRUE)[[1L]][[1L]]
      start_over(
        pool_group_keys(current$explore, input$group, current$selection),
        from = sprintf("exploratory grouping (%s)", model_label_ui(model))
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

    list(sent = sent)
  })
}
