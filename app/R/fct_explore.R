#' Datasets offered on the Explore tab
#'
#' The values are valid R identifiers because esquisse uses them in the plot
#' code it generates. `model_residuals` avoids shadowing `stats::residuals`.
#' @noRd
explore_dataset_choices <- c(
  "Observed dose groups" = "observed",
  "Fitted curves" = "curves",
  "Residuals" = "model_residuals"
)

#' Data frames behind the Explore tab
#'
#' - `observed`: the standardized dose groups.
#' - `curves`: fitted dose-response curves per model (`estimate`). With a
#'   bootstrap attached there is one row per dose and confidence level, with
#'   `lower`/`upper` bounds and `level` as a factor (widest interval first); otherwise they are `NA`.
#' - `model_residuals`: observed groups with fitted values and deviance
#'   residuals per model.
#'
#' @param analysis A `qdr_analysis`.
#' @return A named list of data frames.
#' @noRd
explore_datasets <- function(analysis) {
  fits <- analysis$fits
  models <- names(fits)

  curves <- do.call(rbind, lapply(models, function(name) {
    fit <- fits[[name]]
    # A bootstrap with too few converged replicates cannot give bands; fall back
    # to the bare curve instead of breaking the whole tab.
    curve <- tryCatch(
      singlehit::prediction_curve(fit, bootstrap = analysis$bootstraps[[name]]),
      error = function(e) singlehit::prediction_curve(fit)
    )
    cbind(model = model_label_ui(name), as.data.frame(curve), stringsAsFactors = FALSE)
  }))
  # Widest interval first: ggplot draws later groups on top, so the 99% band must
  # come before the 95% band or it hides it (singlehit's own plots do the same).
  levels <- sort(unique(curves$level), decreasing = TRUE)
  # paste0() on a zero-length vector still returns "%", which would add a stray
  # level to an all-NA column when there is no bootstrap.
  labels <- if (length(levels) > 0L) paste0(format(100 * levels, trim = TRUE), "%") else character()
  curves$level <- factor(curves$level, levels = levels, labels = labels)

  model_residuals <- do.call(rbind, lapply(models, function(name) {
    cbind(
      model = model_label_ui(name),
      as.data.frame(generics::augment(fits[[name]])),
      stringsAsFactors = FALSE
    )
  }))

  list(
    observed = as.data.frame(analysis$data),
    curves = curves,
    model_residuals = model_residuals
  )
}
