# The one-draw-per-replicate algorithm that generate_bootstrap_tasks() replaced.
# The vectorized version must reproduce it exactly, so existing seeds keep giving
# the same replicates.
reference_tasks <- function(object, probability, times, seed) {
  generate <- function() {
    lapply(seq_len(times), function(replicate) {
      positive <- stats::rbinom(nrow(object$data), size = object$data$total, prob = probability)
      list(
        replicate = replicate,
        sample_data = tibble::tibble(
          dose = object$data$dose,
          positive = positive,
          negative = object$data$total - positive
        )
      )
    })
  }
  if (is.null(seed)) generate() else withr::with_seed(seed, generate())
}

ward_fit <- function(model = "beta_poisson") {
  data <- read_dose_response(system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"))
  fit_dose_response(data, model)
}

test_that("vectorized task generation reproduces the per-replicate algorithm exactly", {
  fit <- ward_fit()
  for (probability in list(fit$data$response, fit$fitted)) {
    expect_identical(
      generate_bootstrap_tasks(fit, probability, 60L, seed = 7L),
      reference_tasks(fit, probability, 60L, seed = 7L)
    )
  }
})

test_that("every model type and a single replicate produce the same tasks", {
  for (model in c("exponential", "exact_beta_poisson")) {
    fit <- ward_fit(model)
    expect_identical(
      generate_bootstrap_tasks(fit, fit$fitted, 25L, seed = 3L),
      reference_tasks(fit, fit$fitted, 25L, seed = 3L)
    )
  }
  fit <- ward_fit()
  expect_identical(
    generate_bootstrap_tasks(fit, fit$data$response, 1L, seed = 11L),
    reference_tasks(fit, fit$data$response, 1L, seed = 11L)
  )
})

test_that("tasks are numbered, well formed samples of the original groups", {
  fit <- ward_fit()
  tasks <- generate_bootstrap_tasks(fit, fit$data$response, 40L, seed = NULL)
  expect_length(tasks, 40L)
  expect_equal(vapply(tasks, function(task) task$replicate, integer(1)), 1:40)
  for (task in tasks) {
    sample <- task$sample_data
    expect_s3_class(sample, "tbl_df")
    expect_equal(sample$dose, fit$data$dose)
    expect_equal(sample$positive + sample$negative, fit$data$total)
    expect_true(all(sample$positive >= 0 & sample$negative >= 0))
  }
})

test_that("a seed makes the tasks reproducible and leaves the global random stream alone", {
  fit <- ward_fit()
  withr::local_seed(99)
  before <- .Random.seed
  first <- generate_bootstrap_tasks(fit, fit$data$response, 30L, seed = 5L)
  expect_identical(.Random.seed, before)
  expect_identical(first, generate_bootstrap_tasks(fit, fit$data$response, 30L, seed = 5L))
  expect_false(identical(first, generate_bootstrap_tasks(fit, fit$data$response, 30L, seed = 6L)))
})

test_that("bootstrap replicates are unchanged end to end", {
  fit <- ward_fit()
  tasks <- reference_tasks(fit, fit$data$response, 12L, seed = 21L)
  expected <- dplyr::bind_rows(lapply(tasks, function(task) bootstrap_refit(fit, task$sample_data, task$replicate)))
  boot <- bootstrap_dose_response(fit, times = 12L, seed = 21L, backend = "sequential")
  expect_equal(boot$deviance, expected$deviance)
  expect_equal(boot$converged, expected$converged)
})
