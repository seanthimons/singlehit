slow_task <- function() {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  list(fit = fit, task = generate_bootstrap_tasks(fit, fit$data$response, 1L, 7L)[[1]])
}

test_that("a refit that runs past the limit becomes a non-converged row with the reason", {
  s <- slow_task()
  local_mocked_bindings(refit_sample = function(...) repeat Sys.sleep(0.02))

  started <- proc.time()[["elapsed"]]
  row <- bootstrap_refit(s$fit, s$task$sample_data, 5L, time_limit = 0.3)

  expect_lt(proc.time()[["elapsed"]] - started, 5)
  expect_false(row$converged)
  expect_equal(row$replicate, 5L)
  expect_match(row$error, "0.3-second replicate time limit", fixed = TRUE)
  expect_true(is.na(row$ed50))
})

test_that("a refit that swallows the interruption and finishes late still counts as timed out", {
  s <- slow_task()
  real <- refit_sample(s$fit, s$task$sample_data)
  # The exact model's optimiser tries several starting values inside tryCatch(),
  # which can absorb the first interruption and carry on.
  local_mocked_bindings(refit_sample = function(...) {
    tryCatch(repeat Sys.sleep(0.02), error = function(e) NULL)
    real
  })

  row <- bootstrap_refit(s$fit, s$task$sample_data, 1L, time_limit = 0.3)

  expect_false(row$converged)
  expect_match(row$error, "replicate time limit")
})

test_that("a fast refit under a finite limit matches the unlimited result and clears the limit", {
  s <- slow_task()
  unlimited <- bootstrap_refit(s$fit, s$task$sample_data, 1L)
  limited <- bootstrap_refit(s$fit, s$task$sample_data, 1L, time_limit = 30)
  expect_equal(limited, unlimited)
  expect_true(limited$converged)
})

test_that("the limit does not leak into later work in the same process", {
  s <- slow_task()
  real <- refit_sample(s$fit, s$task$sample_data)
  # An instant refit, so the limit is still armed when bootstrap_refit() returns.
  local_mocked_bindings(refit_sample = function(...) real)
  bootstrap_refit(s$fit, s$task$sample_data, 1L, time_limit = 0.2)
  for (i in 1:12) Sys.sleep(0.05)
  expect_true(TRUE)
})

test_that("the limit cannot fire while a failure row is being built", {
  s <- slow_task()
  real_parameters <- model_parameters
  local_mocked_bindings(
    refit_sample = function(...) stop("optimiser failed"),
    # Slow work inside the error handler, past the limit.
    model_parameters = function(model) {
      for (i in 1:10) Sys.sleep(0.03)
      real_parameters(model)
    }
  )
  row <- bootstrap_refit(s$fit, s$task$sample_data, 1L, time_limit = 0.1)
  expect_equal(row$error, "optimiser failed")
  expect_false(row$converged)
})

test_that("the limit covers the refit only, not the work after it", {
  s <- slow_task()
  real <- refit_sample(s$fit, s$task$sample_data)
  real_effective_dose <- effective_dose
  local_mocked_bindings(
    refit_sample = function(...) real,
    effective_dose = function(...) {
      for (i in 1:10) Sys.sleep(0.03)
      real_effective_dose(...)
    }
  )
  row <- bootstrap_refit(s$fit, s$task$sample_data, 1L, time_limit = 0.1)
  expect_true(row$converged)
  expect_true(is.na(row$error))
})

test_that("the sequential path and the mirai worker apply the limit to every replicate", {
  s <- slow_task()
  tasks <- generate_bootstrap_tasks(s$fit, s$fit$data$response, 2L, 7L)
  local_mocked_bindings(refit_sample = function(...) repeat Sys.sleep(0.02))

  sequential <- run_sequential_bootstrap(tasks, s$fit, time_limit = 0.2)
  batched <- bootstrap_worker()(tasks, s$fit, NULL, 0.2)

  expect_false(any(sequential$converged))
  expect_match(sequential$error, "time limit")
  expect_equal(tibble::as_tibble(batched), tibble::as_tibble(sequential))
})

test_that("replicate_time_limit is validated, and Inf means no limit", {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  for (bad in list(0, -1, NA_real_, "30", c(10, 20), NULL)) {
    expect_error(
      prepare_bootstrap(fit, 3L, "observed", 1L, "sequential", NULL, NULL, bad),
      "replicate_time_limit"
    )
  }
  # With no bootstraps requested, only the early check in analyze_dose_response()
  # can reject the value.
  expect_error(
    analyze_dose_response(ward_fixture(), bootstrap_times = 0L, exact_bootstrap_times = 0L, replicate_time_limit = 0),
    "replicate_time_limit"
  )
  expect_equal(prepare_bootstrap(fit, 3L, "observed", 1L, "sequential", NULL, NULL)$time_limit, 30)
  expect_equal(prepare_bootstrap(fit, 3L, "observed", 1L, "sequential", NULL, NULL, Inf)$time_limit, Inf)
})

test_that("the limit is sent to the mirai daemons with each batch", {
  skip_if_not_installed("mirai")
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  inputs <- prepare_bootstrap(fit, 3L, "observed", 42L, "mirai", NULL, NULL, 12)
  captured <- NULL
  local_mocked_bindings(
    daemons_set = function(...) TRUE,
    mirai_map = function(.x, .f, .args, ...) {
      captured <<- .args
      list()
    },
    .package = "mirai"
  )
  start_mirai_bootstrap(inputs, fit)
  expect_equal(captured$time_limit, 12)
})

test_that("the limit is passed through the public bootstrap and analysis functions", {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  seen <- c()
  local_mocked_bindings(
    run_sequential_bootstrap = function(tasks, object, time_limit = Inf) {
      seen <<- c(seen, time_limit)
      purrr::map_dfr(tasks, function(task) bootstrap_refit(object, task$sample_data, task$replicate, time_limit))
    }
  )
  bootstrap_dose_response(fit, times = 2L, backend = "sequential", replicate_time_limit = 11)
  bootstrap_dose_response_models(list(beta_poisson = fit), times = 2L, backend = "sequential", replicate_time_limit = 12)
  analyze_dose_response(
    ward_fixture(),
    models = c("exponential", "beta_poisson"),
    bootstrap_times = 2L,
    backend = "sequential",
    replicate_time_limit = 13
  )
  expect_equal(seen[1:2], c(11, 12))
  expect_true(all(seen[3:4] == 13))
})
