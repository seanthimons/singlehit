#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "singlehit",
      bslib::nav_panel(
        "Data",
        bslib::layout_sidebar(sidebar = bslib::sidebar(mod_upload_ui("upload")), mod_upload_table_ui("upload"))
      ),
      bslib::nav_panel(
        "Fit",
        bslib::layout_sidebar(sidebar = bslib::sidebar(mod_fit_ui("fit")), mod_fit_results_ui("fit"))
      ),
      bslib::nav_panel("Compare", mod_compare_ui("compare")),
      bslib::nav_panel("Plots", mod_figures_ui("figures"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "singlehit"
    )
  )
}
