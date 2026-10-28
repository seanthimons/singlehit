#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    # A pulse bar across the top while the server is busy, and spinners on
    # outputs that are recalculating. R is single threaded, so a long fit or a
    # run starting can make the app unresponsive; this says why.
    shiny::useBusyIndicators(),
    bslib::page_navbar(
      id = "main",
      title = "singlehit",
      bslib::nav_panel(
        "Data",
        bslib::layout_sidebar(sidebar = bslib::sidebar(mod_upload_ui("upload")), mod_upload_table_ui("upload"))
      ),
      bslib::nav_panel("Library", mod_library_ui("library")),
      bslib::nav_panel("Pooling", mod_pooling_ui("pooling")),
      bslib::nav_panel(
        "Fit",
        bslib::layout_sidebar(sidebar = bslib::sidebar(mod_fit_ui("fit")), mod_fit_results_ui("fit"))
      ),
      bslib::nav_panel("Compare", mod_compare_ui("compare")),
      bslib::nav_panel("Bootstrap", mod_bootstrap_ui("bootstrap")),
      bslib::nav_panel("Plots", mod_figures_ui("figures")),
      bslib::nav_panel("Explore", mod_explore_ui("explore"))
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
