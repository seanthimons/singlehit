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
build_figure <- function(analysis, type = c("overlay", "panels", "single"), model = NULL,
                         components = NULL, colour_by = NULL) {
  type <- match.arg(type)
  plot <- switch(
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
  if (!is.null(components) && !is.null(colour_by) && colour_by %in% substudy_fields) {
    plot <- recolour_observed(plot, analysis$data, components, colour_by, aesthetic = if (type == "overlay") "fill" else "colour")
  }
  plot
}

#' Sub-study fields the observed points of pooled data can be coloured by
#' @noRd
substudy_fields <- c(
  "Experiment" = "experiment",
  "Pathogen" = "pathogen",
  "Strain or isolate" = "strain",
  "Host" = "host",
  "Exposure route" = "route",
  "Outcome" = "outcome"
)

#' Whether per-experiment dose groups add up to the analysed data
#'
#' Pooled data reach Fit with counts at shared doses added together; the
#' components are the same counts per experiment. They belong to an analysis
#' only when their per-dose sums equal its data, which guards against pairing
#' an analysis with another dataset's components.
#' @noRd
components_match <- function(components, data) {
  if (is.null(components) || is.null(data) || nrow(components) == 0L) {
    return(FALSE)
  }
  sums <- stats::aggregate(cbind(positive, negative) ~ dose, data = components, FUN = sum)
  observed <- as.data.frame(data)[c("dose", "positive", "negative")]
  observed <- observed[order(observed$dose), , drop = FALSE]
  sums <- sums[order(sums$dose), , drop = FALSE]
  nrow(sums) == nrow(observed) &&
    isTRUE(all.equal(sums$dose, observed$dose)) &&
    all(sums$positive == observed$positive) &&
    all(sums$negative == observed$negative)
}

#' Replace a singlehit plot's observed points with per-experiment points
#'
#' singlehit draws the observed dose groups as one point layer. For pooled
#' data those points merge experiments at shared doses, so they are swapped
#' for one point per experiment and dose, coloured by a sub-study field and
#' sized by the number of subjects. The layer is found by its geometry and
#' its data; if it is not there the plot is returned unchanged.
#'
#' @param aesthetic `"colour"`, or `"fill"` for the overlay, where colour
#'   already distinguishes the models.
#' @noRd
recolour_observed <- function(plot, data, components, field, aesthetic = c("colour", "fill")) {
  aesthetic <- match.arg(aesthetic)
  is_observed <- vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint") && is.data.frame(layer$data) &&
      "dose" %in% names(layer$data) && all(layer$data$dose %in% data$dose)
  }, logical(1L))
  if (!any(is_observed)) {
    return(plot)
  }
  plot$layers <- plot$layers[-which(is_observed)[[1L]]]
  components$group_value <- components[[field]]
  label <- names(substudy_fields)[substudy_fields == field][[1L]]
  # No facet column: a layer without `model` is drawn in every model's panel.
  # Each experiment is nudged by a fixed fraction of a log10 unit, the same in
  # every panel, so experiments that share a dose and response do not hide
  # each other.
  experiments <- unique(components$experiment)
  offset <- (match(components$experiment, experiments) - (length(experiments) + 1) / 2) * 0.02
  components$plot_dose <- components$dose * 10^offset
  points <- if (aesthetic == "colour") {
    ggplot2::geom_point(
      data = components,
      ggplot2::aes(x = .data$plot_dose, y = .data$response, colour = .data$group_value, size = .data$total),
      alpha = 0.8
    )
  } else {
    ggplot2::geom_point(
      data = components,
      ggplot2::aes(x = .data$plot_dose, y = .data$response, fill = .data$group_value, size = .data$total),
      shape = 21,
      colour = "grey20",
      alpha = 0.8
    )
  }
  # In the overlay, colour already shows the models; a separate palette keeps
  # the experiments' fills from matching a model's line.
  fill_scale <- if (aesthetic == "fill") ggplot2::scale_fill_viridis_d(end = 0.9)
  plot + points + fill_scale +
    ggplot2::scale_size_area(max_size = 5, name = "Subjects", breaks = function(x) unique(round(pretty(x)))) +
    ggplot2::labs(!!aesthetic := label)
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

#' The view to switch to when confidence bands become available
#'
#' The overlay never draws bands, so someone looking at it right after a
#' bootstrap would see no change and conclude the bands did not apply.
#'
#' @param type The currently selected view.
#' @param has_bands Whether the analysis now carries bootstraps.
#' @return The view to switch to, or `NULL` to leave the choice alone.
#' @noRd
view_for_bands <- function(type, has_bands) {
  if (isTRUE(has_bands) && identical(type, "overlay")) "panels" else NULL
}
