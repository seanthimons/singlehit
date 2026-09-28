#' Create prediction curves and bootstrap intervals
#'
#' @param object A `qdr_fit` object.
#' @param bootstrap Optional matching `qdr_bootstrap` object.
#' @param levels Confidence levels for bootstrap pointwise intervals.
#' @param points Number of log-spaced doses in the curve.
#' @param dose_range Optional positive length-two dose range.
#'
#' @return A tibble containing dose, fitted response, confidence level, and
#'   pointwise bounds. Without a bootstrap object, level and bounds are `NA`.
#' @export
prediction_curve <- function(
  object,
  bootstrap = NULL,
  levels = c(0.95, 0.99),
  points = 200L,
  dose_range = NULL
) {
  check_qdr_fit(object)
  validate_curve_arguments(points, dose_range)
  if (is.null(dose_range)) {
    dose_range <- range(object$data$dose) * c(0.1, 10)
  }
  dose <- exp(seq(log(dose_range[[1L]]), log(dose_range[[2L]]), length.out = points))
  estimate <- stats::predict(object, dose)
  if (is.null(bootstrap)) {
    return(tibble::tibble(dose = dose, estimate = estimate, level = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  check_matching_bootstrap(object, bootstrap)
  validate_confidence_levels(levels)
  successful <- bootstrap |>
    tibble::as_tibble() |>
    dplyr::filter(.data$converged)
  if (nrow(successful) < 2L) {
    stop("At least two converged bootstrap replicates are required for confidence curves.", call. = FALSE)
  }
  simulated <- bootstrap_prediction_matrix(object$model, successful, dose)

  purrr::map_dfr(sort(levels, decreasing = TRUE), function(level) {
    tail_probability <- (1 - level) / 2
    bounds <- apply(
      simulated,
      2L,
      stats::quantile,
      probs = c(tail_probability, 1 - tail_probability),
      na.rm = TRUE,
      names = FALSE
    )
    tibble::tibble(
      dose = dose,
      estimate = estimate,
      level = level,
      lower = bounds[1L, ],
      upper = bounds[2L, ]
    )
  })
}

# Reconstruct the response curve for every bootstrap draw. Delegates to
# model_probability() (the same dispatch fitting uses), so it is correct for any
# registered model -- exponential, approximate and exact beta-Poisson -- with no
# per-model algebra. Returns a draws x dose matrix.
bootstrap_prediction_matrix <- function(model, estimates, dose) {
  parameter_names <- model_parameters(model)
  draws <- as.matrix(estimates[, parameter_names, drop = FALSE])
  predictions <- vapply(
    seq_len(nrow(draws)),
    function(i) model_probability(model, dose, stats::setNames(draws[i, ], parameter_names)),
    numeric(length(dose))
  )
  t(predictions)
}

validate_curve_arguments <- function(points, dose_range) {
  if (length(points) != 1L || !is.numeric(points) || !is.finite(points) || points < 2 || points != floor(points)) {
    stop("`points` must be a whole number of at least two.", call. = FALSE)
  }
  if (
    !is.null(dose_range) &&
      (!is.numeric(dose_range) ||
        length(dose_range) != 2L ||
        any(!is.finite(dose_range)) ||
        any(dose_range <= 0) ||
        dose_range[[1L]] >= dose_range[[2L]])
  ) {
    stop("`dose_range` must contain two increasing positive values.", call. = FALSE)
  }
  invisible(NULL)
}

check_matching_bootstrap <- function(fit, bootstrap) {
  check_qdr_bootstrap(bootstrap)
  bootstrap_fit <- attr(bootstrap, "fit")
  if (!identical(fit$model, bootstrap_fit$model)) {
    stop("The bootstrap model does not match the fitted model.", call. = FALSE)
  }
  invisible(bootstrap)
}

#' Plot a fitted dose-response model
#'
#' @param object A `qdr_fit` object.
#' @param bootstrap Optional matching `qdr_bootstrap` object.
#' @inheritParams prediction_curve
#'
#' @return A ggplot object.
#' @export
plot_dose_response <- function(object, bootstrap = NULL, levels = c(0.95, 0.99), points = 200L) {
  curve <- prediction_curve(object, bootstrap = bootstrap, levels = levels, points = points)
  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = object$data,
      ggplot2::aes(x = .data$dose, y = .data$response),
      shape = 17,
      size = 2.5
    )
  if (!is.null(bootstrap)) {
    curve <- dplyr::mutate(curve, interval = factor(.data$level, levels = sort(unique(.data$level), decreasing = TRUE)))
    plot <- plot +
      ggplot2::geom_ribbon(
        data = curve,
        ggplot2::aes(x = .data$dose, ymin = .data$lower, ymax = .data$upper, fill = .data$interval),
        alpha = 0.2
      ) +
      ggplot2::labs(fill = "Confidence level")
  }
  plot +
    ggplot2::geom_line(
      data = dplyr::distinct(curve, .data$dose, .data$estimate),
      ggplot2::aes(x = .data$dose, y = .data$estimate),
      linewidth = 0.8
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = "Dose",
      y = "Probability of response",
      title = paste(model_label(object$model), "dose-response model")
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @exportS3Method ggplot2::autoplot
autoplot.qdr_fit <- function(object, bootstrap = NULL, ...) {
  plot_dose_response(object, bootstrap = bootstrap, ...)
}

#' @exportS3Method ggplot2::autoplot
autoplot.qdr_bootstrap <- function(object, ...) {
  check_qdr_bootstrap(object)
  fit <- attr(object, "fit")
  data <- tibble::as_tibble(object) |>
    dplyr::filter(.data$converged)
  parameter_names <- model_parameters(fit$model)
  title <- paste(model_label(fit$model), "parameter uncertainty")
  if (length(parameter_names) == 1L) {
    return(
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[parameter_names[[1L]]]])) +
        ggplot2::geom_histogram(bins = 30, color = "white", fill = "#287D8E") +
        ggplot2::labs(x = parameter_names[[1L]], y = "Bootstrap replicates", title = title) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[parameter_names[[1L]]]], y = .data[[parameter_names[[2L]]]])) +
    ggplot2::geom_point(alpha = 0.45, color = "#B13C2E") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = parameter_names[[1L]], y = parameter_names[[2L]], title = title) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @exportS3Method ggplot2::autoplot
autoplot.qdr_analysis <- function(object, levels = c(0.95, 0.99), points = 200L, ...) {
  if (!inherits(object, "qdr_analysis")) {
    stop("`object` must be a qdr_analysis object.", call. = FALSE)
  }
  curves <- purrr::imap_dfr(object$fits, function(fit, name) {
    bootstrap <- object$bootstraps[[name]] %||% NULL
    prediction_curve(fit, bootstrap = bootstrap, levels = levels, points = points) |>
      dplyr::mutate(model = model_label(fit$model))
  })
  observations <- purrr::map_dfr(unique(curves$model), function(model_name) {
    dplyr::mutate(object$data, model = model_name)
  })
  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = observations,
      ggplot2::aes(x = .data$dose, y = .data$response),
      shape = 17,
      size = 2.2
    )
  if (length(object$bootstraps) > 0L) {
    curves <- dplyr::mutate(
      curves,
      interval = factor(.data$level, levels = sort(unique(.data$level), decreasing = TRUE))
    )
    plot <- plot +
      ggplot2::geom_ribbon(
        data = curves,
        ggplot2::aes(x = .data$dose, ymin = .data$lower, ymax = .data$upper, fill = .data$interval),
        alpha = 0.2
      ) +
      ggplot2::labs(fill = "Confidence level")
  }
  plot +
    ggplot2::geom_line(
      data = dplyr::distinct(curves, .data$model, .data$dose, .data$estimate),
      ggplot2::aes(x = .data$dose, y = .data$estimate),
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$model)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Dose", y = "Probability of response") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Overlay fitted models on one dose-response plot
#'
#' Shows each fitted model against the observed responses without confidence
#' intervals. The standard [ggplot2::autoplot()] view remains faceted.
#'
#' @param object A `qdr_analysis` object.
#' @param points Number of log-spaced doses in each curve.
#'
#' @return A ggplot object.
#' @export
plot_model_overlay <- function(object, points = 200L) {
  if (!inherits(object, "qdr_analysis")) {
    stop("`object` must be a qdr_analysis object.", call. = FALSE)
  }
  curves <- purrr::map_dfr(object$fits, function(fit) {
    prediction_curve(fit, points = points) |>
      dplyr::mutate(model = model_label(fit$model))
  })

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = object$data,
      ggplot2::aes(x = .data$dose, y = .data$response),
      shape = 17,
      size = 2.2
    ) +
    ggplot2::geom_line(
      data = curves,
      ggplot2::aes(x = .data$dose, y = .data$estimate, color = .data$model, linetype = .data$model),
      linewidth = 0.8
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Dose", y = "Probability of response", color = "Model", linetype = "Model") +
    ggplot2::theme_minimal(base_size = 11)
}
