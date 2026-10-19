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
}
