#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  user_on_start <- onStart
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      # The bootstrap workers start lazily on the first run, so a deployment that
      # never bootstraps never claims them; this only makes sure they stop with
      # the app.
      onStart = function() {
        shiny::onStop(stop_bootstrap_workers)
        if (is.function(user_on_start)) {
          user_on_start()
        }
      },
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
