#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  data <- mod_upload_server("upload")
  analysis <- mod_fit_server("fit", data)
  mod_compare_server("compare", analysis)
  # Plots and Explore get the analysis with the bootstraps attached once a run
  # finishes; Compare does not use them.
  analysis_with_bands <- mod_bootstrap_server("bootstrap", analysis)
  mod_figures_server("figures", analysis_with_bands)
  mod_explore_server("explore", analysis_with_bands)
}
