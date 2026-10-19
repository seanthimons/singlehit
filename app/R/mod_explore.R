#' Explore module UI
#'
#' Hosts esquisse so plots can be built by dragging variables onto aesthetics.
#' The header keeps only "show data": importing replaces the validated data,
#' the close button would stop the whole app, the editing buttons change a local
#' copy that the next analysis silently discards, and opening Settings resets
#' the aesthetics list this module sets.
#'
#' @param id Module id.
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
        selectInput(ns("dataset"), "Data to explore", choices = explore_dataset_choices, width = "320px")
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

    dataset
  })
}
