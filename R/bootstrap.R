#' Bootstrap a fitted dose-response model
#'
#' Generates grouped binomial bootstrap samples and refits the model. The
#' `"observed"` resampling method preserves the legacy CAMRA behavior by using
#' each dose group's observed response probability. The `"fitted"` method is a
#' model-based parametric bootstrap. Mirai runs show progress as batches
#' finish.
#'
#' @param object A `qdr_fit` object.
#' @param times Number of bootstrap replicates.
#' @param resample Either `"observed"` or `"fitted"`.
#' @param seed Optional integer random seed.
#' @param backend Execution backend. `"auto"` uses mirai above 1,000 replicates
#'   when it is installed and otherwise runs sequentially. `"sequential"` runs
#'   refits in the current R process. `"mirai"` distributes refits to existing
#'   daemons or a temporary local pool. When loaded with `pkgload::load_all()`,
#'   the same checkout is loaded on each worker.
#' @param compute Optional mirai compute profile name. Ignored by the sequential
#'   backend.
#' @param workers Number of temporary local mirai daemons. `NULL` uses 75% of
#'   detected physical cores, with a minimum of one. Ignored when using existing
#'   daemons or the sequential backend.
#'
#' @return A `qdr_bootstrap` tibble with one row per replicate.
#' @export
bootstrap_dose_response <- function(
  object,
  times = 10000L,
  resample = c("observed", "fitted"),
  seed = NULL,
  backend = c("auto", "sequential", "mirai"),
  compute = NULL,
  workers = NULL
) {
  inputs <- prepare_bootstrap(object, times, resample, seed, backend, compute, workers)
  backend <- inputs$backend
  if (backend == "sequential") {
    estimates <- run_sequential_bootstrap(inputs$tasks, object)
    return(new_qdr_bootstrap(estimates, object, inputs$resample, backend))
  }
  collect_bootstrap(start_mirai_bootstrap(inputs, object))
}

#' Start a non-blocking dose-response bootstrap
#'
#' Starts mirai bootstrap refits and immediately returns a job. Call
#' [collect_bootstrap()] to wait for and retrieve the completed result.
#'
#' @inheritParams bootstrap_dose_response
#'
#' @return A `qdr_bootstrap_job` for [collect_bootstrap()].
#' @export
bootstrap_dose_response_async <- function(
  object,
  times = 10000L,
  resample = c("observed", "fitted"),
  seed = NULL,
  compute = NULL,
  workers = NULL
) {
  inputs <- prepare_bootstrap(object, times, resample, seed, "mirai", compute, workers)
  start_mirai_bootstrap(inputs, object)
}

#' Collect a non-blocking bootstrap job
#'
#' Shows mirai's progress indicator while waiting for batches to finish.
#'
#' @param job A `qdr_bootstrap_job` returned by
#'   [bootstrap_dose_response_async()].
#'
#' @return A completed `qdr_bootstrap` tibble.
#' @export
collect_bootstrap <- function(job) {
  if (!inherits(job, "qdr_bootstrap_job")) {
    stop("`job` must be a qdr_bootstrap_job.", call. = FALSE)
  }
  if (job$owns_daemons) {
    on.exit(mirai::daemons(0L, .compute = job$compute), add = TRUE)
  }
  results <- mirai::collect_mirai(job$map, options = ".progress")
  estimates <- bind_bootstrap_results(results)
  new_qdr_bootstrap(estimates, job$fit, job$resample, "mirai")
}

#' Check the progress of a non-blocking bootstrap job
#'
#' Reports how many batches of a [bootstrap_dose_response_async()] job have
#' finished. Polling never waits for batches and does not change the job, so it
#' is safe to call repeatedly from an interactive app.
#'
#' @param job A `qdr_bootstrap_job` returned by [bootstrap_dose_response_async()].
#'
#' @return A list with `done` (finished batches), `total` (batches in the job),
#'   and `finished` (`TRUE` once every batch has resolved). Each batch holds up
#'   to 20 replicates. Call [collect_bootstrap()] to retrieve the result.
#' @details Failed batches count as finished; [collect_bootstrap()] reports them
#'   as errors. Polling does not stop the mirai daemons that the job owns.
#' @export
bootstrap_progress <- function(job) {
  if (!inherits(job, "qdr_bootstrap_job")) {
    stop("`job` must be a qdr_bootstrap_job.", call. = FALSE)
  }
  pending <- vapply(job$map, mirai::unresolved, logical(1))
  total <- length(pending)
  done <- total - sum(pending)
  list(done = as.integer(done), total = as.integer(total), finished = done == total)
}

prepare_bootstrap <- function(object, times, resample, seed, backend, compute, workers) {
  check_qdr_fit(object)
  resample <- match.arg(resample, c("observed", "fitted"))
  backend <- match.arg(backend, c("auto", "sequential", "mirai"))
  requested_backend <- backend
  if (length(times) != 1L || !is.numeric(times) || !is.finite(times) || times < 1 || times != floor(times)) {
    stop("`times` must be a positive whole number.", call. = FALSE)
  }
  if (!is.null(compute) && (!is.character(compute) || length(compute) != 1L || is.na(compute) || !nzchar(compute))) {
    stop("`compute` must be NULL or one non-empty mirai compute profile name.", call. = FALSE)
  }
  if (
    !is.null(workers) &&
      (length(workers) != 1L || !is.numeric(workers) || !is.finite(workers) || workers < 1 || workers != floor(workers))
  ) {
    stop("`workers` must be NULL or a positive whole number.", call. = FALSE)
  }
  times <- as.integer(times)
  backend <- select_bootstrap_backend(backend, times)
  if (requested_backend == "auto" && backend == "sequential" && times > 1000L) {
    warning("Package 'mirai' is not installed; running bootstraps sequentially.", call. = FALSE)
  }
  probability <- if (resample == "observed") object$data$response else object$fitted
  list(
    tasks = generate_bootstrap_tasks(object, probability, times, seed),
    resample = resample,
    backend = backend,
    compute = compute,
    workers = if (is.null(workers)) NULL else as.integer(workers)
  )
}

select_bootstrap_backend <- function(backend, times, mirai_available = requireNamespace("mirai", quietly = TRUE)) {
  if (backend != "auto") {
    return(backend)
  }
  if (times > 1000L && mirai_available) "mirai" else "sequential"
}

recommended_bootstrap_workers <- function(cores = parallel::detectCores(logical = FALSE)) {
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    cores <- parallel::detectCores(logical = TRUE)
  }
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    return(1L)
  }
  max(1L, as.integer(floor(0.75 * cores)))
}

new_qdr_bootstrap <- function(estimates, object, resample, backend) {
  parameter_columns <- c(model_parameters(object$model), "ed10", "ed50")
  structure(
    estimates,
    class = c("qdr_bootstrap", class(estimates)),
    fit = object,
    resample = resample,
    backend = backend,
    parameter_columns = parameter_columns
  )
}

generate_bootstrap_tasks <- function(object, probability, times, seed) {
  generate <- function() {
    groups <- nrow(object$data)
    total <- object$data$total
    # One vectorized draw for every replicate. rbinom() consumes the random
    # stream in order, so this is identical to drawing each replicate in turn,
    # and far cheaper than building a tibble per replicate with tibble().
    draws <- matrix(
      stats::rbinom(groups * times, size = rep(total, times), prob = rep(probability, times)),
      nrow = groups
    )
    lapply(seq_len(times), function(replicate) {
      positive <- draws[, replicate]
      list(
        replicate = replicate,
        sample_data = tibble::new_tibble(
          list(dose = object$data$dose, positive = positive, negative = total - positive),
          nrow = groups
        )
      )
    })
  }

  if (is.null(seed)) generate() else withr::with_seed(seed, generate())
}

run_sequential_bootstrap <- function(tasks, object) {
  purrr::map_dfr(tasks, function(task) {
    bootstrap_refit(object, task$sample_data, task$replicate)
  })
}

start_mirai_bootstrap <- function(inputs, object) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("Package 'mirai' is required for `backend = \"mirai\"`.", call. = FALSE)
  }
  compute <- inputs$compute
  owns_daemons <- !mirai::daemons_set(.compute = compute)
  if (owns_daemons) {
    if (is.null(compute)) {
      compute <- basename(tempfile("singlehit-"))
    }
    workers <- if (is.null(inputs$workers)) recommended_bootstrap_workers() else inputs$workers
    mirai::daemons(workers, .compute = compute)
  }
  # Keep a 10,000-replicate run to 500 queued jobs while retaining load balancing.
  batches <- split(inputs$tasks, ceiling(seq_along(inputs$tasks) / 20L))
  dev_path <- if (requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("singlehit")) {
    getNamespaceInfo("singlehit", "path")
  } else {
    NULL
  }
  mapped <- tryCatch(
    mirai::mirai_map(
      batches,
      function(batch, original_fit, dev_path) {
        if (!is.null(dev_path) && !pkgload::is_dev_package("singlehit")) {
          pkgload::load_all(dev_path, quiet = TRUE)
        }
        dplyr::bind_rows(lapply(batch, function(task) {
          singlehit:::bootstrap_refit(original_fit, task$sample_data, task$replicate)
        }))
      },
      .args = list(original_fit = object, dev_path = dev_path),
      .compute = compute
    ),
    error = function(cnd) {
      if (owns_daemons) {
        mirai::daemons(0L, .compute = compute)
      }
      stop(cnd)
    }
  )
  structure(
    list(
      map = mapped,
      fit = object,
      resample = inputs$resample,
      compute = compute,
      owns_daemons = owns_daemons
    ),
    class = "qdr_bootstrap_job"
  )
}

bind_bootstrap_results <- function(results) {
  failed <- vapply(results, function(result) inherits(result, c("miraiError", "errorValue")), logical(1))
  if (any(failed)) {
    first_failure <- results[[which(failed)[[1L]]]]
    stop(sprintf("A mirai bootstrap task failed: %s", as.character(first_failure)), call. = FALSE)
  }
  dplyr::bind_rows(results)
}

# Refit a resampled grouped dataset. Resampled rows line up one-to-one with the
# fitted rows (one per dose for a single dataset, stacked rows for a pooled fit),
# so they are fit as-is. Going through as_dose_response() would merge shared doses.
refit_sample <- function(original_fit, sample_data) {
  total <- sample_data$positive + sample_data$negative
  data <- tibble::tibble(
    dose = sample_data$dose,
    positive = sample_data$positive,
    negative = sample_data$negative,
    total = total,
    response = sample_data$positive / total
  )
  validate_dose_response_groups(data)
  fit_core(data, original_fit$model, start = original_fit$coefficients)
}

bootstrap_refit <- function(original_fit, sample_data, replicate) {
  tryCatch(
    {
      fit <- suppressWarnings(refit_sample(original_fit, sample_data))
      values <- c(
        list(
          model = original_fit$model,
          replicate = replicate,
          converged = fit$convergence == 0L,
          log_lik = fit$log_lik,
          deviance = fit$deviance
        ),
        as.list(fit$coefficients),
        list(
          ed10 = effective_dose(fit, 0.1),
          ed50 = effective_dose(fit, 0.5),
          error = NA_character_
        )
      )
      tibble::as_tibble_row(values)
    },
    error = function(cnd) {
      parameters <- stats::setNames(
        as.list(rep(NA_real_, length(model_parameters(original_fit$model)))),
        model_parameters(original_fit$model)
      )
      values <- c(
        list(
          model = original_fit$model,
          replicate = replicate,
          converged = FALSE,
          log_lik = NA_real_,
          deviance = NA_real_
        ),
        parameters,
        list(ed10 = NA_real_, ed50 = NA_real_, error = conditionMessage(cnd))
      )
      tibble::as_tibble_row(values)
    }
  )
}

#' Bootstrap both dose-response models
#'
#' @param object A `qdr_model_set` or list of `qdr_fit` objects.
#' @inheritParams bootstrap_dose_response
#'
#' @return A named list of `qdr_bootstrap` objects.
#' @export
bootstrap_dose_response_models <- function(
  object,
  times = 10000L,
  resample = c("observed", "fitted"),
  seed = NULL,
  backend = c("auto", "sequential", "mirai"),
  compute = NULL,
  workers = NULL
) {
  fits <- as_fit_list(object)
  resample <- match.arg(resample)
  backend <- match.arg(backend)
  purrr::imap(fits, function(fit, index) {
    model_seed <- if (is.null(seed)) NULL else seed + match(index, names(fits)) - 1L
    bootstrap_dose_response(
      fit,
      times = times,
      resample = resample,
      seed = model_seed,
      backend = backend,
      compute = compute,
      workers = workers
    )
  })
}

#' Bootstrap percentile confidence intervals
#'
#' @param object A `qdr_bootstrap` object.
#' @param levels Confidence levels strictly between zero and one.
#'
#' @return A tibble with percentile interval bounds for each parameter and
#'   effective dose.
#' @export
bootstrap_confint <- function(object, levels = c(0.95, 0.99)) {
  check_qdr_bootstrap(object)
  validate_confidence_levels(levels)
  parameter_columns <- attr(object, "parameter_columns")
  estimates <- object |>
    tibble::as_tibble() |>
    dplyr::filter(.data$converged) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(parameter_columns),
      names_to = "term",
      values_to = "estimate"
    )

  purrr::map_dfr(sort(levels), function(level) {
    tail_probability <- (1 - level) / 2
    estimates |>
      dplyr::group_by(.data$model, .data$term) |>
      dplyr::summarise(
        level = level,
        lower = stats::quantile(.data$estimate, tail_probability, na.rm = TRUE, names = FALSE),
        upper = stats::quantile(.data$estimate, 1 - tail_probability, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  })
}

validate_confidence_levels <- function(levels) {
  if (!is.numeric(levels) || length(levels) == 0L || any(!is.finite(levels)) || any(levels <= 0 | levels >= 1)) {
    stop("`levels` must contain values strictly between zero and one.", call. = FALSE)
  }
  invisible(levels)
}

check_qdr_bootstrap <- function(object) {
  if (!inherits(object, "qdr_bootstrap")) {
    stop("`object` must be a qdr_bootstrap object.", call. = FALSE)
  }
  invisible(object)
}

#' @export
print.qdr_bootstrap <- function(x, ...) {
  model <- unique(x$model)
  failures <- sum(!x$converged)
  cat(sprintf(
    "<qdr_bootstrap> %s model: %d replicates [%s]",
    model_label(model),
    nrow(x),
    attr(x, "backend") %||% "sequential"
  ))
  if (failures > 0L) {
    cat(sprintf(" (%d did not converge)", failures))
  }
  cat("\n")
  print(bootstrap_confint(x, levels = 0.95), n = Inf)
  invisible(x)
}
