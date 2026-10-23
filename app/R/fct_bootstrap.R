#' Replicate limits for the bootstrap controls
#'
#' The minimum is 100 because a 99% percentile interval from fewer draws is
#' meaningless. The exact model is far slower per replicate, so it has a smaller
#' ceiling: the shared pool is first-in first-out, and a very long exact run
#' would hold up every other session.
#'
#' The ceiling matches singlehit's own default of 10,000. Starting a run prepares
#' every replicate on the app's single R process, which blocked it for 17 s at
#' 10,000 replicates (two models) before singlehit drew the replicates in one
#' vectorized call and stopped shipping the whole task list with every batch; it
#' is now about a second. See dev/benchmark_bootstrap.R.
#' @noRd
bootstrap_limits <- list(min = 100L, max = 10000L, exact_max = 1000L)

#' Gentle defaults for the bootstrap controls
#' @noRd
bootstrap_defaults <- list(times = 1000L, exact_times = 200L)

check_count <- function(x, label, lower, upper) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || x != floor(x) || x < lower || x > upper) {
    stop(
      sprintf(
        "%s must be a whole number between %s and %s.",
        label,
        format(lower, big.mark = ","),
        format(upper, big.mark = ",")
      ),
      call. = FALSE
    )
  }
  as.integer(x)
}

#' Work out what to run for each fitted model
#'
#' Seeds follow `analyze_dose_response()`: `seed + index - 1` over the fitted
#' models, so a run can be reproduced from the exported script.
#'
#' @param analysis A `qdr_analysis`.
#' @param times Replicates for the exponential and beta-Poisson models.
#' @param exact_times Replicates for the exact model; used only if it was fitted.
#' @param seed Optional whole number; `NULL` or `NA` for none.
#' @return A named list (by model) of `list(model, times, seed)`.
#' @noRd
bootstrap_plan <- function(analysis, times, exact_times = NULL, seed = NULL) {
  models <- names(analysis$fits)
  times <- check_count(times, "Replicates", bootstrap_limits$min, bootstrap_limits$max)
  if ("exact_beta_poisson" %in% models) {
    if (is.null(exact_times)) {
      exact_times <- bootstrap_defaults$exact_times
    }
    exact_times <- check_count(exact_times, "Exact-model replicates", bootstrap_limits$min, bootstrap_limits$exact_max)
  }
  if (is.null(seed) || length(seed) != 1L || is.na(seed)) {
    seed <- NULL
  } else {
    seed <- check_count(seed, "The seed", 0L, .Machine$integer.max - length(models))
  }
  plan <- lapply(seq_along(models), function(i) {
    list(
      model = models[[i]],
      times = if (models[[i]] == "exact_beta_poisson") exact_times else times,
      seed = if (is.null(seed)) NULL else seed + i - 1L
    )
  })
  names(plan) <- models
  plan
}

#' Summarize the progress of several jobs
#'
#' @param progress Named list of `singlehit::bootstrap_progress()` results.
#' @return A list with a per-model data frame and the overall fraction.
#' @noRd
progress_summary <- function(progress) {
  done <- vapply(progress, function(p) p$done, integer(1))
  total <- vapply(progress, function(p) p$total, integer(1))
  list(
    models = data.frame(
      model = names(progress),
      done = unname(done),
      total = unname(total),
      fraction = ifelse(total > 0L, unname(done) / unname(total), 1),
      stringsAsFactors = FALSE
    ),
    fraction = if (sum(total) > 0L) sum(done) / sum(total) else 1,
    finished = all(done == total)
  )
}

#' Split bootstraps into those that can draw bands and those that cannot
#'
#' `prediction_curve()` stops when fewer than two replicates converged, which
#' would break the Plots tab, so such a bootstrap is reported and not attached.
#'
#' @param boots Named list of `qdr_bootstrap` objects.
#' @return A list with `attached` (named list) and `dropped` (model names).
#' @noRd
attachable_bootstraps <- function(boots) {
  ok <- vapply(boots, function(boot) sum(boot$converged, na.rm = TRUE) >= 2L, logical(1))
  list(attached = boots[ok], dropped = names(boots)[!ok])
}

#' How many replicates converged, per model
#' @noRd
convergence_table <- function(boots) {
  data.frame(
    Model = model_label_ui(names(boots)),
    `Converged replicates` = vapply(
      boots,
      function(boot) sprintf("%d of %d", sum(boot$converged, na.rm = TRUE), nrow(boot)),
      character(1)
    ),
    check.names = FALSE,
    row.names = NULL
  )
}

#' Percentile intervals for one bootstrap, one row per quantity
#'
#' Reshapes `singlehit::bootstrap_confint()` (long, one row per quantity and
#' level) into one row per quantity with a column per confidence level.
#' @noRd
bootstrap_interval_table <- function(boot) {
  ci <- as.data.frame(singlehit::bootstrap_confint(boot))
  quantities <- unique(ci$term)
  labels <- c(n50 = "N50", ed10 = "ED10", ed50 = "ED50")
  shown <- ifelse(quantities %in% names(labels), labels[quantities], quantities)
  out <- data.frame(
    Model = model_label_ui(ci$model[[1L]]),
    Quantity = unname(shown),
    stringsAsFactors = FALSE
  )
  for (level in sort(unique(ci$level))) {
    part <- ci[ci$level == level, , drop = FALSE]
    idx <- match(quantities, part$term)
    out[[paste0(format(100 * level, trim = TRUE), "% interval")]] <- paste0(
      "(",
      format_number(part$lower[idx]),
      ", ",
      format_number(part$upper[idx]),
      ")"
    )
  }
  out
}

#' Interval tables for all models, stacked
#' @noRd
bootstrap_tables <- function(boots) {
  do.call(rbind, unname(lapply(boots, bootstrap_interval_table)))
}

#' Attach finished bootstraps to the analysis they were computed from
#'
#' Bands must never describe a different fit than the one on screen, so the
#' bootstraps are attached only when `base` is identical to the analysis they
#' were run against.
#'
#' @param base The current `qdr_analysis`, or `NULL`.
#' @param boots Named list of finished `qdr_bootstrap` objects (possibly empty).
#' @param bootstrapped_base The analysis the bootstraps were run against.
#' @return `base` with the bootstraps attached, or `base` unchanged.
#' @noRd
attach_bands <- function(base, boots, bootstrapped_base) {
  if (!is.null(base) && length(boots) > 0L && identical(bootstrapped_base, base)) {
    singlehit::add_bootstrap(base, boots)
  } else {
    base
  }
}
