#' Figure views offered on the Plots tab
#' @noRd
figure_types <- c(
  "Model overlay" = "overlay",
  "One panel per model" = "panels",
  "Single model" = "single"
)

#' Build one of the standard singlehit figures
#'
#' Uses only exported singlehit plotting functions. Confidence bands appear in
#' the `"panels"` and `"single"` views when the analysis carries bootstraps; the
#' overlay never draws bands.
#'
#' @param analysis A `qdr_analysis`.
#' @param type One of `"overlay"`, `"panels"`, or `"single"`.
#' @param model Model name for `type = "single"`.
#' @return A ggplot object.
#' @noRd
build_figure <- function(analysis, type = c("overlay", "panels", "single"), model = NULL) {
  type <- match.arg(type)
  switch(
    type,
    overlay = singlehit::plot_model_overlay(analysis),
    panels = ggplot2::autoplot(analysis),
    single = {
      if (is.null(model) || !model %in% names(analysis$fits)) {
        stop("Choose one of the fitted models.", call. = FALSE)
      }
      singlehit::plot_dose_response(analysis$fits[[model]], bootstrap = analysis$bootstraps[[model]])
    }
  )
}

#' Named model choices for a select input
#' @noRd
model_choices <- function(analysis) {
  models <- names(analysis$fits)
  names(models) <- model_label_ui(models)
  models
}

#' Clamp a numeric input into a range, falling back to a default
#'
#' Numeric inputs can arrive as `NULL` or `NA` while the user is typing.
#'
#' @noRd
bounded_number <- function(x, lower, upper, default) {
  if (is.null(x) || length(x) != 1L || !is.numeric(x) || is.na(x)) {
    return(default)
  }
  min(max(x, lower), upper)
}
