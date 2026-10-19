#' Figures module UI
#'
#' @param id Module id.
#' @noRd
mod_figures_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      radioButtons(ns("type"), "View", choices = figure_types),
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
#' @noRd
mod_figures_server <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
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
      build_figure(analysis(), input$type, input$model)
    })

    output$empty <- renderUI({
      if (is.null(analysis())) {
        p(class = "text-muted", "Run the analysis on the Fit tab to see figures.")
      }
    })
    output$plot <- renderPlot(figure(), res = 96)

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
