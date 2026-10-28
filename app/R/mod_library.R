#' Library module UI
#'
#' Explore the QMRA wiki workbook loaded on the Data tab: filter its
#' experiments, draw their preferred models faceted by metadata, send one
#' experiment back to the Data and Fit tabs, or choose several to test for
#' poolability on the Pooling tab.
#'
#' @param id Module id.
#' @noRd
mod_library_ui <- function(id) {
  ns <- NS(id)
  filter <- function(field, label) {
    selectizeInput(ns(paste0("filter_", field)), label, choices = character(), multiple = TRUE, options = list(placeholder = "All"))
  }
  # Not fillable: the plot sits above a long table, and in a fillable layout
  # the page squeezes the plot to fit the window until base graphics fails
  # with "figure margins too large". Here the main panel scrolls instead.
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 320,
      h6("Filter experiments"),
      filter("category", "Pathogen group"),
      filter("pathogen", "Pathogen"),
      filter("outcome_group", "Outcome group"),
      filter("dose_unit", "Dose unit"),
      filter("host_group", "Host"),
      filter("route_group", "Exposure route"),
      filter("model_label", "Preferred model"),
      checkboxInput(ns("hide_wiki_pooled"), "Hide entries the wiki already pooled", value = FALSE),
      hr(),
      h6("Plot"),
      selectInput(ns("facet"), "Facet by", choices = c("None" = "none", library_fields), selected = "outcome_group"),
      selectInput(ns("colour"), "Colour by", choices = library_fields, selected = "category"),
      checkboxInput(ns("free_x"), "Separate dose axis per facet", value = FALSE)
    ),
    uiOutput(ns("empty")),
    uiOutput(ns("status")),
    plotOutput(ns("plot"), height = "620px", fill = FALSE),
    uiOutput(ns("send_panel")),
    div(
      class = "mt-2 mb-3 p-3 border rounded",
      h6("Pooling set"),
      p(
        class = "text-muted small",
        "Experiments to test for poolability on the Pooling tab. Only experiments that can be fitted and that the wiki",
        "has not already pooled are offered. The list follows the filters but keeps what you have already chosen."
      ),
      div(
        class = "d-flex flex-wrap align-items-end gap-3",
        selectizeInput(
          ns("pool_set"),
          NULL,
          choices = character(),
          multiple = TRUE,
          width = "520px",
          options = list(placeholder = "Choose two or more experiments")
        ),
        actionButton(ns("pool"), "Test poolability", icon = icon("layer-group"), class = "btn-outline-primary mb-3")
      )
    ),
    tableOutput(ns("table"))
  )
}

#' Library module server
#'
#' @param id Module id.
#' @param rows Reactive returning the workbook rows from [read_qmra_workbook()],
#'   or `NULL` before a workbook is loaded on the Data tab.
#' @return A list of reactives: `sent`, `NULL` until an experiment is sent and
#'   then a list of `data`, `label`, and `sent` (a counter that changes on every
#'   send); `pool_set`, `NULL` until "Test poolability" is pressed and then a
#'   list of `keys` and a `sent` counter; and `experiments`, the
#'   [library_experiments()] table, so other tabs need not rebuild it.
#' @noRd
mod_library_server <- function(id, rows) {
  moduleServer(id, function(input, output, session) {
    filters <- c("category", "pathogen", "outcome_group", "dose_unit", "host_group", "route_group", "model_label")
    loaded <- reactive({
      req(rows())
      rows()
    })
    # Checking and refitting every experiment is the slow step (seconds for the
    # full workbook), so it runs once per workbook, not on every filter change.
    experiments <- reactive(library_experiments(loaded()))
    curves <- reactive(library_curves(loaded()))

    observeEvent(experiments(), {
      for (field in filters) {
        updateSelectizeInput(session, paste0("filter_", field), choices = sort(unique(experiments()[[field]])), selected = character())
      }
    })

    filtered <- reactive({
      out <- experiments()
      for (field in filters) {
        chosen <- input[[paste0("filter_", field)]]
        if (length(chosen) > 0L) {
          out <- out[out[[field]] %in% chosen, , drop = FALSE]
        }
      }
      if (isTRUE(input$hide_wiki_pooled)) {
        out <- out[!out$wiki_pooled, , drop = FALSE]
      }
      out
    })

    output$empty <- renderUI({
      if (is.null(rows())) {
        p(class = "text-muted", "Load the QMRA wiki workbook (.xlsx), or the sample workbook, on the Data tab to browse its experiments.")
      }
    })
    output$status <- renderUI({
      shown <- filtered()
      drawn <- length(intersect(shown$experiment_key, curves()$experiment_key))
      p(
        class = "text-muted",
        sprintf(
          "%d of %d experiments match the filters; %d have a curve to draw.",
          nrow(shown), nrow(experiments()), drawn
        )
      )
    })

    output$plot <- renderPlot(
      {
        shown <- filtered()
        validate(need(any(curves()$experiment_key %in% shown$experiment_key), "No curves to draw for these filters."))
        library_plot(curves(), shown, facet = input$facet, colour = input$colour, free_x = isTRUE(input$free_x))
      },
      res = 96
    )

    output$send_panel <- renderUI({
      shown <- filtered()
      req(nrow(shown) > 0L)
      choices <- stats::setNames(shown$experiment_key, library_experiment_label(shown))
      keep <- intersect(isolate(input$experiment), choices)
      div(
        class = "d-flex flex-wrap align-items-end gap-3 mt-3",
        selectInput(
          session$ns("experiment"),
          "Experiment to analyse",
          choices = choices,
          selected = if (length(keep) > 0L) keep else NULL,
          width = "520px"
        ),
        uiOutput(session$ns("send_button"))
      )
    })

    chosen <- reactive({
      req(input$experiment)
      match <- experiments()[experiments()$experiment_key == input$experiment, , drop = FALSE]
      req(nrow(match) == 1L)
      match
    })

    output$send_button <- renderUI({
      experiment <- chosen()
      if (experiment$fittable) {
        actionButton(session$ns("send"), "Send to Data and Fit", icon = icon("arrow-right"), class = "btn-primary mb-3")
      } else {
        div(class = "alert alert-warning small py-2 mb-3", "Cannot be fitted: ", experiment$problem)
      }
    })

    output$table <- renderTable({
      shown <- filtered()
      req(nrow(shown) > 0L)
      data.frame(
        Experiment = shown$experiment_id,
        Pathogen = shown$pathogen,
        Outcome = shown$response_type,
        `Dose unit` = shown$dose_unit,
        Host = shown$host,
        Route = shown$route,
        `Preferred model` = shown$model_label,
        Doses = shown$doses,
        Subjects = format(shown$subjects, big.mark = ","),
        `Wiki-pooled` = ifelse(shown$wiki_pooled, "yes", ""),
        Fittable = ifelse(shown$fittable, "yes", "no"),
        check.names = FALSE
      )
    })

    observeEvent(filtered(), {
      all <- experiments()
      selected <- intersect(isolate(input$pool_set), all$experiment_key)
      shown <- filtered()
      eligible <- shown$experiment_key[shown$fittable & !shown$wiki_pooled]
      offered <- all[all$experiment_key %in% union(eligible, selected), , drop = FALSE]
      choices <- stats::setNames(offered$experiment_key, library_experiment_label(offered))
      updateSelectizeInput(session, "pool_set", choices = choices, selected = selected)
    })

    pool_sent <- reactiveVal(NULL)
    pool_count <- reactiveVal(0L)
    observeEvent(input$pool, {
      req(length(input$pool_set) > 0L)
      pool_count(pool_count() + 1L)
      pool_sent(list(keys = input$pool_set, sent = pool_count()))
    })

    sent <- reactiveVal(NULL)
    count <- reactiveVal(0L)
    observeEvent(input$send, {
      experiment <- chosen()
      req(experiment$fittable)
      count(count() + 1L)
      sent(list(
        data = library_experiment_data(loaded(), experiment$experiment_key),
        label = library_experiment_label(experiment),
        sent = count()
      ))
    })
    list(sent = sent, pool_set = pool_sent, experiments = experiments)
  })
}
