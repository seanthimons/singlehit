#' Run the complete microbial dose-response analysis
#'
#' Standardizes the input, evaluates its trend, fits the requested models,
#' compares their fit, and optionally bootstraps uncertainty.
#'
#' @param data Grouped dose-response data accepted by [as_dose_response()].
#' @param models Character vector of models to fit and compare, a subset of
#'   `"exponential"`, `"beta_poisson"`, and `"exact_beta_poisson"`. Defaults to
#'   the exponential and approximate beta-Poisson models; add
#'   `"exact_beta_poisson"` to include the exact model.
#' @param bootstrap_times Number of bootstrap replicates for the exponential and
#'   approximate beta-Poisson models. Use zero to skip their bootstrapping.
#' @param exact_bootstrap_times Number of bootstrap replicates for the exact
#'   beta-Poisson model, which is roughly an order of magnitude slower to fit.
#'   Only used when `"exact_beta_poisson"` is in `models`; use zero to fit and
#'   compare the exact model without bootstrapping it.
#' @param resample Bootstrap method passed to [bootstrap_dose_response()].
#' @param seed Optional integer random seed.
#' @param backend Bootstrap execution backend passed to
#'   [bootstrap_dose_response()].
#' @param compute Optional mirai compute profile name.
#' @param workers Number of temporary local mirai daemons, passed to
#'   [bootstrap_dose_response()].
#' @param check_trend Warn when an increasing trend is not detected.
#' @param alpha Significance level for trend and goodness-of-fit tests.
#' @param replicate_time_limit Seconds one bootstrap replicate may run before it
#'   is recorded as not converged, passed to [bootstrap_dose_response()].
#'
#' @return A `qdr_analysis` object containing standardized data, trend test,
#'   model fits, diagnostics, parsed assessment, comparison, and bootstrap
#'   results.
#' @export
analyze_dose_response <- function(
  data,
  models = c("exponential", "beta_poisson"),
  bootstrap_times = 10000L,
  exact_bootstrap_times = 10000L,
  resample = c("observed", "fitted"),
  seed = NULL,
  backend = c("auto", "sequential", "mirai"),
  compute = NULL,
  workers = NULL,
  check_trend = TRUE,
  alpha = 0.05,
  replicate_time_limit = 30
) {
  data <- as_dose_response(data)
  resample <- match.arg(resample)
  backend <- match.arg(backend)
  validate_bootstrap_times(bootstrap_times, "bootstrap_times")
  validate_bootstrap_times(exact_bootstrap_times, "exact_bootstrap_times")
  validate_replicate_time_limit(replicate_time_limit)
  fits <- fit_dose_response_models(data, models = models, check_trend = check_trend, alpha = alpha)

  # Bootstrap each fitted model with its own replicate count: the exact model is
  # far slower to fit, so it gets a separate (opt-out via zero) budget. Models
  # with a zero budget are fit and compared but not bootstrapped.
  bootstraps <- purrr::imap(fits, function(fit, name) {
    times <- if (fit$model == "exact_beta_poisson") exact_bootstrap_times else bootstrap_times
    if (times <= 0L) {
      return(NULL)
    }
    model_seed <- if (is.null(seed)) NULL else seed + match(name, names(fits)) - 1L
    bootstrap_dose_response(
      fit,
      times = times,
      resample = resample,
      seed = model_seed,
      backend = backend,
      compute = compute,
      workers = workers,
      replicate_time_limit = replicate_time_limit
    )
  })
  bootstraps <- purrr::compact(bootstraps)

  goodness <- goodness_of_fit(fits, alpha = alpha)
  comparison <- compare_dose_response_models(fits, alpha = alpha)
  assessment <- build_model_assessment(goodness, comparison)

  structure(
    list(
      data = data,
      trend = attr(fits, "trend"),
      fits = fits,
      goodness_of_fit = goodness,
      comparison = comparison,
      assessment = assessment,
      bootstraps = bootstraps
    ),
    class = "qdr_analysis"
  )
}

#' Attach bootstrap results to an analysis
#'
#' Adds bootstrap replicates to an existing `qdr_analysis`, for example after a
#' fast analysis run with `bootstrap_times = 0`. Plotting the returned analysis
#' with [ggplot2::autoplot()] then draws confidence bands.
#'
#' @param object A `qdr_analysis` object.
#' @param bootstraps A non-empty named list of `qdr_bootstrap` objects, named by
#'   model, such as the output of [bootstrap_dose_response_models()]. Names must
#'   match models in `object`.
#'
#' @return The `qdr_analysis` with the supplied bootstraps added or replaced.
#'   Models not named in `bootstraps` keep their existing bootstrap results.
#' @details Each bootstrap must come from the matching fit in `object`: the same
#'   model, data, and coefficients. Bootstraps computed from another dataset or
#'   fit are rejected. Pass `analysis$fits` to [bootstrap_dose_response_models()],
#'   which expects a model set or a list of fits rather than a whole analysis.
#' @examples
#' analysis <- analyze_dose_response(read_dose_response(
#'   system.file("extdata", "Ward_rotavirus.txt", package = "singlehit")
#' ), bootstrap_times = 0, check_trend = FALSE)
#' boots <- bootstrap_dose_response_models(analysis$fits, times = 50, seed = 1)
#' analysis <- add_bootstrap(analysis, boots)
#' ggplot2::autoplot(analysis)
#' @export
add_bootstrap <- function(object, bootstraps) {
  if (!inherits(object, "qdr_analysis")) {
    stop("`object` must be a qdr_analysis object.", call. = FALSE)
  }
  if (
    !is.list(bootstraps) ||
      is.data.frame(bootstraps) ||
      length(bootstraps) == 0L ||
      is.null(names(bootstraps)) ||
      any(!nzchar(names(bootstraps))) ||
      anyDuplicated(names(bootstraps)) > 0L
  ) {
    stop("`bootstraps` must be a non-empty named list of qdr_bootstrap objects.", call. = FALSE)
  }
  unknown <- setdiff(names(bootstraps), names(object$fits))
  if (length(unknown) > 0L) {
    stop(
      sprintf("No fitted model named %s in `object`.", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  for (name in names(bootstraps)) {
    bootstrap <- bootstraps[[name]]
    if (!inherits(bootstrap, "qdr_bootstrap")) {
      stop(sprintf("`bootstraps$%s` must be a qdr_bootstrap object.", name), call. = FALSE)
    }
    fit <- object$fits[[name]]
    bootstrap_fit <- attr(bootstrap, "fit")
    matches <- identical(fit$model, bootstrap_fit$model) &&
      isTRUE(all.equal(fit$data, bootstrap_fit$data)) &&
      isTRUE(all.equal(fit$coefficients, bootstrap_fit$coefficients))
    if (!matches) {
      stop(
        sprintf("The bootstrap for %s was not computed from this analysis's fit.", model_label(fit$model)),
        call. = FALSE
      )
    }
  }
  object$bootstraps[names(bootstraps)] <- bootstraps
  object
}

validate_bootstrap_times <- function(times, name) {
  if (length(times) != 1L || !is.numeric(times) || !is.finite(times) || times < 0 || times != floor(times)) {
    stop(sprintf("`%s` must be a non-negative whole number.", name), call. = FALSE)
  }
  invisible(times)
}

#' @export
print.qdr_analysis <- function(x, ...) {
  cat("<qdr_analysis> microbial dose-response analysis\n")
  cat(sprintf(
    "Trend: Z = %.3f, one-sided p = %.4g\n\n",
    x$trend$statistic,
    x$trend$p_value
  ))
  cat("Model assessment:\n")
  print(
    x$assessment[, c("model", "recommendation", "conclusion")],
    n = Inf
  )
  cat("\nModel comparison:\n")
  print(x$comparison, n = Inf)
  if (length(x$bootstraps) > 0L) {
    cat(sprintf("\nBootstrap replicates per model: %d\n", nrow(x$bootstraps[[1L]])))
  }
  invisible(x)
}
