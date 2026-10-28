#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Data loads both the workbook (explored on Library) and the dataset Fit
  # analyses; Library sends an experiment back. The reactiveVal breaks that
  # cycle: Data is built first and reads whatever Library last sent.
  sent <- reactiveVal(NULL)
  upload <- mod_upload_server("upload", incoming = sent)
  library_sent <- mod_library_server("library", upload$workbook)
  data <- upload$data
  observeEvent(library_sent(), {
    sent(library_sent())
    # A sent experiment replaces the current dataset; show it there first.
    bslib::nav_select("main", "Data", session = session)
  })
  observeEvent(upload$workbook_loaded(), bslib::nav_select("main", "Library", session = session), ignoreInit = TRUE)
  analysis <- mod_fit_server("fit", data)
  mod_compare_server("compare", analysis)
  # Plots and Explore get the analysis with the bootstraps attached once a run
  # finishes; Compare does not use them.
  analysis_with_bands <- mod_bootstrap_server("bootstrap", analysis)
  mod_figures_server("figures", analysis_with_bands)
  mod_explore_server("explore", analysis_with_bands)
}
