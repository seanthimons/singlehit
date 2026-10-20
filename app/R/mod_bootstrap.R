#' Bootstrap module UI
#'
#' @param id Module id.
#' @noRd
mod_bootstrap_ui <- function(id) {
  ns <- NS(id)
  # fillable = FALSE: the results are tall, so let the page scroll and give each
  # card its natural height instead of squeezing the cards and plots to fit.
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      numericInput(
        ns("times"),
        "Replicates",
        value = bootstrap_defaults$times,
        min = bootstrap_limits$min,
        max = bootstrap_limits$max,
        step = 100
      ),
      uiOutput(ns("exact_input")),
      radioButtons(
        ns("resample"),
        "Resampling",
        choices = c("Observed response probabilities" = "observed", "Fitted model (parametric)" = "fitted")
      ),
      numericInput(ns("seed"), "Random seed (optional)", value = NA, min = 0, step = 1),
      actionButton(ns("run"), "Run bootstrap", class = "btn-primary"),
      actionButton(ns("cancel"), "Cancel", class = "btn-outline-secondary"),
      p(
        class = "text-muted small mt-3",
        "Runs in the background on a shared pool of workers, so you can keep using the other tabs.",
        "When it finishes, confidence bands appear on the Plots and Explore tabs.",
        "Larger runs take a few seconds to start, during which the app is busy."
      )
    ),
    uiOutput(ns("status")),
    uiOutput(ns("results"))
  )
}

#' One progress bar per model
#'
#' Per-model bars matter because the exact model's batches are far slower, so a
#' single overall bar would stall at the end.
#' @noRd
progress_bars <- function(progress, elapsed) {
  summary <- progress_summary(progress)
  tagList(
    p(class = "mb-2", sprintf("Running: %.0f s elapsed", elapsed)),
    lapply(seq_len(nrow(summary$models)), function(i) {
      row <- summary$models[i, ]
      percent <- round(100 * row$fraction)
      div(
        class = "mb-2",
        div(class = "small", model_label_ui(row$model)),
        div(
          class = "progress",
          role = "progressbar",
          `aria-valuenow` = percent,
          `aria-valuemin` = 0,
          `aria-valuemax` = 100,
          div(class = "progress-bar", style = sprintf("width: %d%%;", percent), sprintf("%d%%", percent))
        )
      )
    })
  )
}

#' Bootstrap module server
#'
#' One job per fitted model runs on the shared mirai pool. Progress is polled
#' without blocking; results are collected only once every job has finished. A
#' run is cancelled when the analysis changes, when Cancel is pressed, and when
#' the session ends, so no workers keep computing for a result nobody can see.
#'
#' @param id Module id.
#' @param analysis Reactive returning a `qdr_analysis`, or `NULL`.
#' @return A reactive returning the analysis with the completed bootstraps
#'   attached (so Plots and Explore draw bands), or the base analysis otherwise.
#' @noRd
mod_bootstrap_server <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      status = "idle",
      base = NULL,
      progress = NULL,
      boots = list(),
      dropped = character(),
      warnings = character(),
      message = NULL,
      started = NULL,
      elapsed = NULL,
      tick = 0L
    )
    jobs <- reactiveVal(list())

    cancel_jobs <- function() {
      for (job in isolate(jobs())) {
        try(mirai::stop_mirai(job$map), silent = TRUE)
      }
      jobs(list())
    }

    reset <- function() {
      state$status <- "idle"
      state$base <- NULL
      state$progress <- NULL
      state$boots <- list()
      state$dropped <- character()
      state$warnings <- character()
      state$message <- NULL
    }

    fail <- function(message) {
      cancel_jobs()
      state$status <- "error"
      state$message <- message
    }

    # A different analysis means different fits: stop any run and drop results.
    observeEvent(analysis(), {
      cancel_jobs()
      reset()
    }, ignoreNULL = FALSE)

    session$onSessionEnded(function() isolate(cancel_jobs()))

    # Starts one job per model. The shared pool is (re)started first, so no job
    # ever owns it and finishing one cannot shut it down under another session.
    launch <- function(base, plan, resample) {
      started <- list()
      tryCatch(
        {
          start_bootstrap_workers()
          for (item in plan) {
            started[[item$model]] <- singlehit::bootstrap_dose_response_async(
              base$fits[[item$model]],
              times = item$times,
              resample = resample,
              seed = item$seed,
              compute = bootstrap_compute_profile
            )
          }
          started
        },
        error = function(e) {
          for (job in started) {
            try(mirai::stop_mirai(job$map), silent = TRUE)
          }
          e
        }
      )
    }

    observeEvent(input$run, {
      base <- analysis()
      if (is.null(base)) {
        showNotification("Run the analysis on the Fit tab first.", type = "warning")
      } else if (identical(state$status, "running")) {
        showNotification("A bootstrap is already running.", type = "warning")
      } else {
        plan <- tryCatch(
          bootstrap_plan(base, input$times, input$exact_times, input$seed),
          error = function(e) e
        )
        if (inherits(plan, "error")) {
          reset()
          state$status <- "error"
          state$message <- conditionMessage(plan)
        } else {
          # Preparing the replicates blocks the app for a few seconds on larger
          # runs, so say so instead of looking frozen.
          started <- withProgress(
            message = "Preparing bootstrap replicates",
            detail = "Large runs take a few seconds to start.",
            value = 0.5,
            launch(base, plan, input$resample)
          )
          if (inherits(started, "error")) {
            reset()
            state$status <- "error"
            state$message <- paste("The bootstrap could not be started:", conditionMessage(started))
          } else {
            reset()
            jobs(started)
            state$base <- base
            state$started <- Sys.time()
            state$progress <- lapply(started, singlehit::bootstrap_progress)
            state$status <- "running"
          }
        }
      }
    })

    observeEvent(input$cancel, {
      if (identical(state$status, "running")) {
        cancel_jobs()
        state$status <- "cancelled"
      }
    })

    finish <- function() {
      collected <- with_captured_warnings(lapply(jobs(), singlehit::collect_bootstrap))
      jobs(list())
      state$warnings <- collected$warnings
      if (!is.null(collected$error)) {
        state$status <- "error"
        state$message <- paste("The bootstrap run failed:", collected$error)
      } else {
        split <- attachable_bootstraps(collected$value)
        state$boots <- split$attached
        state$dropped <- split$dropped
        state$elapsed <- as.numeric(difftime(Sys.time(), state$started, units = "secs"))
        state$status <- "done"
      }
    }

    # Polling reads progress only; it never waits on a job. A cancelled run has
    # status "cancelled", so its stopped jobs (which report as finished) are
    # never collected.
    observe({
      req(identical(state$status, "running"))
      invalidateLater(500, session)
      isolate({
        progress <- tryCatch(lapply(jobs(), singlehit::bootstrap_progress), error = function(e) e)
        if (inherits(progress, "error")) {
          fail(conditionMessage(progress))
        } else {
          state$progress <- progress
          # Re-render the status every poll so the elapsed time keeps moving even
          # when no batch has finished since the last one.
          state$tick <- state$tick + 1L
          if (all(vapply(progress, function(p) p$finished, logical(1)))) {
            finish()
          }
        }
      })
    })

    output$exact_input <- renderUI({
      req(analysis())
      if ("exact_beta_poisson" %in% names(analysis()$fits)) {
        tagList(
          numericInput(
            session$ns("exact_times"),
            "Exact-model replicates",
            value = bootstrap_defaults$exact_times,
            min = bootstrap_limits$min,
            max = bootstrap_limits$exact_max,
            step = 50
          ),
          p(class = "text-muted small", "The exact model is much slower per replicate; start low.")
        )
      }
    })

    output$status <- renderUI({
      if (is.null(analysis())) {
        return(p(class = "text-muted", "Run the analysis on the Fit tab first."))
      }
      switch(
        state$status,
        idle = p(class = "text-muted", "Choose the settings and click Run bootstrap."),
        running = {
          state$tick
          progress_bars(state$progress, as.numeric(difftime(Sys.time(), state$started, units = "secs")))
        },
        done = tagList(
          div(
            class = "alert alert-success",
            sprintf(
              "Bootstrap complete in %.1f seconds. Confidence bands now appear on the Plots and Explore tabs.",
              state$elapsed
            )
          ),
          if (length(state$dropped) > 0L) {
            div(
              class = "alert alert-warning",
              sprintf(
                "Too few replicates converged to draw bands for: %s.",
                paste(model_label_ui(state$dropped), collapse = ", ")
              )
            )
          },
          if (length(state$warnings) > 0L) {
            div(class = "alert alert-warning", tags$ul(lapply(state$warnings, tags$li)))
          }
        ),
        cancelled = div(class = "alert alert-warning", "Bootstrap cancelled. No confidence bands were added."),
        error = div(class = "alert alert-danger", state$message)
      )
    })

    output$results <- renderUI({
      req(identical(state$status, "done"), length(state$boots) > 0L)
      ns <- session$ns
      tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_header("Percentile intervals"),
          tableOutput(ns("intervals")),
          tableOutput(ns("convergence"))
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header("Parameter uncertainty"),
          lapply(names(state$boots), function(model) {
            plotOutput(ns(paste0("plot_", model)), height = "320px", fill = FALSE)
          })
        )
      )
    })
    output$intervals <- renderTable(bootstrap_tables(state$boots))
    output$convergence <- renderTable(convergence_table(state$boots))

    observeEvent(state$boots, {
      for (model in names(state$boots)) {
        local({
          chosen <- model
          output[[paste0("plot_", chosen)]] <- renderPlot(ggplot2::autoplot(state$boots[[chosen]]), res = 96)
        })
      }
    })

    # Held in a reactiveVal, which ignores a set to an identical value, so Plots
    # and Explore only re-render when the analysis they show really changes, not
    # on every status change (Run, Cancel, errors). The exact model's curves are
    # slow to redraw.
    shown <- reactiveVal(NULL)
    observe({
      boots <- if (identical(state$status, "done")) state$boots else list()
      shown(attach_bands(analysis(), boots, state$base))
    })
    reactive(shown())
  })
}
