#' Fit module UI
#'
#' Sidebar inputs for choosing models and running the analysis.
#'
#' @param id Module id.
#' @noRd
mod_fit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(
      ns("models"),
      "Models to fit (at least two)",
      choices = c(
        "Exponential" = "exponential",
        "Beta-Poisson" = "beta_poisson",
        "Exact beta-Poisson (slower)" = "exact_beta_poisson"
      ),
      selected = c("exponential", "beta_poisson")
    ),
    numericInput(ns("alpha"), "Significance level (alpha)", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
    # A task button turns busy the moment it is clicked, in the browser, so it
    # still shows something while the server is too busy to answer.
    bslib::input_task_button(ns("run"), "Run analysis", label_busy = "Fitting models...")
  )
}

#' Fit module results UI
#'
#' @param id Module id.
#' @noRd
mod_fit_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status")),
    uiOutput(ns("trend")),
    uiOutput(ns("tables"))
  )
}

#' Fit module server
#'
#' Runs the no-bootstrap analysis on demand. The result is cleared whenever the
#' data change so the Fit and Compare tabs never describe a different dataset.
#'
#' @param id Module id.
#' @param data Reactive returning standardized data, or `NULL`.
#' @return A reactive returning the `qdr_analysis`, or `NULL`.
#' @noRd
mod_fit_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    result <- reactiveVal(NULL)

    observeEvent(data(), result(NULL), ignoreNULL = FALSE)

    observeEvent(input$run, {
      if (is.null(data())) {
        showNotification("Load data on the Data tab first.", type = "warning")
        return()
      }
      models <- input$models
      alpha <- input$alpha
      problem <- if (length(models) < 2L) {
        "Select at least two models: model comparison needs a pair to compare."
      } else if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
        "The significance level must be a number between 0 and 1."
      }
      if (!is.null(problem)) {
        result(list(value = NULL, error = problem, warnings = character()))
        return()
      }
      withProgress(message = "Fitting models", value = 0.5, {
        result(run_fast_analysis(data(), models, alpha))
      })
    })

    output$status <- renderUI({
      if (is.null(data())) {
        return(p(class = "text-muted", "Load data on the Data tab to begin."))
      }
      run <- result()
      if (is.null(run)) {
        return(p(class = "text-muted", "Choose models and click Run analysis."))
      }
      tagList(
        if (!is.null(run$error)) div(class = "alert alert-danger", run$error),
        if (length(run$warnings) > 0L) {
          div(class = "alert alert-warning", tags$ul(lapply(run$warnings, tags$li)))
        }
      )
    })

    output$trend <- renderUI({
      analysis <- result()$value$analysis
      req(analysis)
      trend <- analysis$trend
      computed <- !is.na(trend$statistic)
      text <- if (computed) {
        sprintf(
          "Z = %s, one-sided p = %s at alpha = %s. %s",
          format_number(trend$statistic),
          format_p(trend$p_value),
          format_number(trend$alpha),
          if (trend$passes) {
            "An increasing dose-response trend is detected."
          } else {
            "No statistically significant increasing trend was detected; interpret the fits with caution."
          }
        )
      } else {
        "The trend test could not be computed because the response does not vary across doses."
      }
      bslib::card(
        bslib::card_header("Dose trend test"),
        div(class = if (computed && trend$passes) "text-success" else "text-warning", text)
      )
    })

    output$tables <- renderUI({
      req(result()$value)
      tagList(
        bslib::card(bslib::card_header("Parameter estimates"), tableOutput(session$ns("parameters"))),
        bslib::card(bslib::card_header("Fit and goodness of fit"), tableOutput(session$ns("fit_stats")))
      )
    })
    output$parameters <- renderTable(result()$value$parameters)
    output$fit_stats <- renderTable(result()$value$fit_stats)

    reactive(result()$value$analysis)
  })
}
