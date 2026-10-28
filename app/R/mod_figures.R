#' Figures module UI
#'
#' @param id Module id.
#' @noRd
mod_figures_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      radioButtons(ns("type"), "View", choices = figure_types),
      uiOutput(ns("band_hint")),
      uiOutput(ns("colour_by_ui")),
      conditionalPanel(
        "input.type == 'single'",
        ns = ns,
        selectInput(ns("model"), "Model", choices = character())
      ),
      hr(),
      h6("Download"),
      numericInput(ns("width"), "Width (inches)", value = 7, min = 2, max = 20, step = 0.5),
      numericInput(ns("height"), "Height (inches)", value = 5, min = 2, max = 20, step = 0.5),
      numericInput(ns("dpi"), "Resolution (dpi)", value = 300, min = 72, max = 600, step = 50),
      uiOutput(ns("download_button"))
    ),
    uiOutput(ns("empty")),
    plotOutput(ns("plot"), height = "520px")
  )
}

#' Figures module server
#'
#' @param id Module id.
#' @param analysis Reactive returning a `qdr_analysis`, or `NULL`.
#' @param components Reactive returning the per-experiment dose groups of
#'   pooled data (see [pool_components()]), or `NULL`.
#' @noRd
mod_figures_server <- function(id, analysis, components = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # Only components that add up to the analysed data are used, so a stale
    # table can never colour another dataset's points.
    matched <- reactive(!is.null(analysis()) && components_match(components(), analysis()$data))
    output$colour_by_ui <- renderUI({
      req(matched())
      keep <- isolate(input$colour_by)
      selectInput(
        session$ns("colour_by"),
        "Colour observed points by",
        choices = c("None (pooled dose groups)" = "", substudy_fields),
        selected = if (is.null(keep)) "" else keep
      )
    })

    # Refresh the model list whenever the analysis changes so a model from an
    # earlier run cannot stay selected.
    observeEvent(
      analysis(),
      {
        choices <- if (is.null(analysis())) character() else model_choices(analysis())
        # Keep the current model when it is still fitted instead of jumping back
        # to the first one on every re-run.
        keep <- intersect(isolate(input$model), choices)
        updateSelectInput(session, "model", choices = choices, selected = if (length(keep) > 0L) keep else NULL)
      },
      ignoreNULL = FALSE
    )

    # Built once and shared by the display and the download: the exact model's
    # curves are slow to compute.
    figure <- reactive({
      req(analysis(), input$type)
      # updateSelectInput is asynchronous, so the previous selection can still
      # be in `input$model` for one render.
      if (input$type == "single") {
        req(input$model %in% names(analysis()$fits))
      }
      build_figure(
        analysis(),
        input$type,
        input$model,
        components = if (matched()) components(),
        colour_by = if (matched() && isTRUE(nzchar(input$colour_by))) input$colour_by
      )
    })

    output$empty <- renderUI({
      if (is.null(analysis())) {
        p(class = "text-muted", "Run the analysis on the Fit tab to see figures.")
      }
    })
    output$plot <- renderPlot(figure(), res = 96)

    # When bands first become available, leave the overlay (which never draws
    # them) for the panel view, so the new bands are visible straight away.
    # Held in a reactiveVal, which ignores a set to an identical value, so the
    # handler below runs only when bands really appear or disappear and does not
    # depend on how often the upstream analysis is recomputed.
    has_bands <- reactiveVal(FALSE)
    observe(has_bands(length(analysis()$bootstraps) > 0L))
    observeEvent(
      has_bands(),
      {
        target <- view_for_bands(input$type, has_bands())
        if (!is.null(target)) {
          updateRadioButtons(session, "type", selected = target)
          showNotification(
            "Confidence bands are drawn in the panel and single-model views, so this view was switched to one panel per model.",
            type = "message",
            duration = 10
          )
        }
      },
      ignoreInit = TRUE
    )

    output$band_hint <- renderUI({
      req(analysis(), input$type)
      hint <- function(text) div(class = "alert alert-info small py-2 mb-2", text)
      if (identical(input$type, "overlay")) {
        hint("The overlay never shows confidence bands. Choose One panel per model or Single model to see them.")
      } else if (identical(input$type, "single") && !is.null(input$model)) {
        if (!input$model %in% names(analysis()$bootstraps)) {
          hint("No confidence bands for this model yet: run the bootstrap.")
        }
      } else if (length(analysis()$bootstraps) == 0L) {
        hint("Run the bootstrap to add confidence bands to this view.")
      }
    })

    output$download_button <- renderUI({
      req(analysis())
      downloadButton(session$ns("download"), "Download PNG")
    })
    output$download <- downloadHandler(
      filename = function() {
        suffix <- if (identical(input$type, "single") && !is.null(input$model)) paste0("-", input$model) else ""
        paste0("singlehit-", input$type, suffix, ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = figure(),
          device = "png",
          width = bounded_number(input$width, 2, 20, 7),
          height = bounded_number(input$height, 2, 20, 5),
          units = "in",
          dpi = bounded_number(input$dpi, 72, 600, 300)
        )
      }
    )
  })
}
