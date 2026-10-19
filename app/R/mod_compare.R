#' Compare module UI
#'
#' @param id Module id.
#' @noRd
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("empty")),
    uiOutput(ns("decision"))
  )
}

#' Compare module server
#'
#' Shows the primary model decision (absolute fit plus the chi-squared deviance
#' comparison) first, then the experimental consensus vote.
#'
#' @param id Module id.
#' @param analysis Reactive returning a `qdr_analysis`, or `NULL`.
#' @noRd
mod_compare_server <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    output$empty <- renderUI({
      if (is.null(analysis())) {
        p(class = "text-muted", "Run the analysis on the Fit tab to see the model comparison.")
      }
    })

    output$decision <- renderUI({
      req(analysis())
      headline <- headline_decision(analysis()$assessment)
      ns <- session$ns
      tagList(
        div(
          class = paste("alert", if (headline$status == "recommended") "alert-success" else "alert-warning"),
          if (headline$status == "recommended") {
            tags$strong(paste0("Recommended model: ", headline$model, ". "))
          },
          headline$text
        ),
        bslib::card(
          bslib::card_header("Model assessment"),
          tableOutput(ns("assessment")),
          uiOutput(ns("assessment_note"))
        ),
        bslib::card(
          bslib::card_header("Consensus selection ", span(class = "badge text-bg-warning", "Experimental")),
          uiOutput(ns("consensus_text")),
          tableOutput(ns("consensus"))
        )
      )
    })

    output$assessment <- renderTable(assessment_table(analysis()$assessment))
    output$assessment_note <- renderUI({
      note <- assessment_note(analysis()$assessment)
      if (!is.null(note)) {
        p(class = "text-muted small", note)
      }
    })

    consensus <- reactive({
      tryCatch(singlehit::consensus_model_decision(analysis()), error = function(e) NULL)
    })
    output$consensus_text <- renderUI({
      if (is.null(consensus())) {
        return(p(class = "text-muted", "A consensus vote needs at least two models."))
      }
      p(class = "text-muted", consensus()$conclusion[[1L]])
    })
    output$consensus <- renderTable({
      req(consensus())
      consensus_table(consensus())
    })
  })
}
