#' Explore module UI
#'
#' Hosts esquisse so plots can be built by dragging variables onto aesthetics.
#' plotly is imported so esquisse finds it: without it esquisse prints a failed
#' requireNamespace() message at startup and shows a "Use {plotly}" switch that
#' does nothing. With it the switch gives hover and zoom on the plots.
#' The header keeps only "show data": importing replaces the validated data,
#' the close button would stop the whole app, the editing buttons change a local
#' copy that the next analysis silently discards, and opening Settings resets
#' the aesthetics list this module sets.
#'
#' @param id Module id.
#' @importFrom plotly plotlyOutput
#' @noRd
mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "output.has_analysis != 'yes'",
      ns = ns,
      p(class = "text-muted", "Run the analysis on the Fit tab to explore the results.")
    ),
    conditionalPanel(
      "output.has_analysis == 'yes'",
      ns = ns,
      div(
        class = "mb-3",
        div(
          class = "d-flex flex-wrap align-items-end gap-3 mb-2",
          selectInput(ns("dataset"), "Data to explore", choices = explore_dataset_choices, width = "320px"),
          conditionalPanel(
            "output.has_bands == 'yes'",
            ns = ns,
            actionButton(
              ns("bands_preset"),
              "Draw confidence bands",
              icon = icon("chart-area"),
              class = "btn-outline-primary mb-3"
            )
          )
        ),
        p(
          class = "text-muted small mb-0",
          paste(
            "Drag variables onto the slots to build a plot. For a log dose axis, open Axes and set the X-axis",
            "transform to log10. Confidence bands (Fitted curves, once a bootstrap has been run): use the Draw",
            "confidence bands button, or map lower to ymin and upper to ymax, choose the Ribbon geometry, and split",
            "by model and level, for example facet by model and fill by level."
          )
        )
      ),
      # container = NULL: esquisse's default container injects a global
      # `html, body { overflow: visible !important }` rule that would leak into
      # the other tabs. `flex: 0 0 auto` stops the fillable navbar page from
      # shrinking this wrapper below esquisse's 400px plot minimum, which would
      # let its control bar cover the bottom of the plot.
      div(
        style = "height: 700px; flex: 0 0 auto;",
        esquisse::esquisse_ui(
          ns("esquisse"),
          header = esquisse::esquisse_header(
            import_data = FALSE,
            update_variable = FALSE,
            create_column = FALSE,
            cut_variable = FALSE,
            update_factor = FALSE,
            settings = FALSE,
            close = FALSE
          ),
          container = NULL
        )
      )
    )
  )
}

#' Explore module server
#'
#' @param id Module id.
#' @param analysis Reactive returning a `qdr_analysis`, or `NULL`.
#' @return A reactive returning the data frame being explored.
#' @noRd
mod_explore_server <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    output$has_analysis <- renderText(if (is.null(analysis())) "no" else "yes")
    outputOptions(output, "has_analysis", suspendWhenHidden = FALSE)

    all_datasets <- reactive(explore_datasets(req(analysis())))
    # Judged on the data, not on whether a bootstrap is attached: a bootstrap
    # with too few converged replicates gives bare curves with no bands to draw.
    output$has_bands <- renderText({
      has_bands <- !is.null(analysis()) && any(!is.na(all_datasets()$curves$lower))
      if (has_bands) "yes" else "no"
    })
    outputOptions(output, "has_bands", suspendWhenHidden = FALSE)
    dataset <- reactive({
      req(input$dataset %in% names(all_datasets()))
      all_datasets()[[input$dataset]]
    })

    # ymin/ymax are not esquisse defaults but are what enables its ribbon geom.
    esquisse::esquisse_server(
      "esquisse",
      data_rv = dataset,
      name = reactive(if (is.null(input$dataset)) "data" else input$dataset),
      default_aes = c("color", "fill", "size", "group", "facet", "ymin", "ymax"),
      import_from = NULL
    )

    # Pre-fill esquisse's slots. Its drag-and-drop, geometry and axis pickers are
    # ordinary shiny inputs, so they can be driven with their public update
    # functions, but each step has to wait for esquisse to react to the one before
    # it: choosing the dataset rebuilds the variable list, and every change to the
    # mapping makes esquisse re-pick the geometry. `preset` records which step is
    # being waited for. Messages go out from onFlushed() so they follow whatever
    # esquisse sent in the same flush.
    preset <- reactiveVal(NULL)
    reported_mapping <- reactive(input[[esquisse_ids$aesthetics]]$target)

    set_geometry_and_axis <- function() {
      preset(NULL)
      session$onFlushed(function() {
        esquisse::updateDropInput(session, esquisse_ids$geom, selected = bands_preset$geom)
        updateSelectizeInput(session, esquisse_ids$x_transform, selected = bands_preset$x_transform)
      }, once = TRUE)
    }
    set_aesthetics <- function() {
      if (mapping_matches(reported_mapping(), bands_preset$aesthetics)) {
        return(set_geometry_and_axis())
      }
      preset("aesthetics")
      session$onFlushed(function() {
        esquisse::updateDragulaInput(session, esquisse_ids$aesthetics, selected = bands_preset$aesthetics)
      }, once = TRUE)
    }

    observeEvent(input$bands_preset, {
      if (identical(input$dataset, "curves")) {
        set_aesthetics()
      } else {
        preset("data")
        updateSelectInput(session, "dataset", selected = "curves")
      }
    })
    # Drop a pending preset whose conditions no longer hold, so it cannot fire
    # later when the user happens to build the same mapping by hand.
    observeEvent(analysis(), preset(NULL), ignoreInit = TRUE)
    observeEvent(input$dataset, if (!identical(input$dataset, "curves")) preset(NULL))
    observeEvent(dataset(), {
      if (identical(preset(), "data") && identical(input$dataset, "curves")) set_aesthetics()
    })
    observeEvent(reported_mapping(), {
      # The dataset change also reports its own (stale) mapping first; only act
      # once esquisse reports the mapping that was sent.
      if (identical(preset(), "aesthetics") && mapping_matches(reported_mapping(), bands_preset$aesthetics)) {
        set_geometry_and_axis()
      }
    })

    dataset
  })
}
