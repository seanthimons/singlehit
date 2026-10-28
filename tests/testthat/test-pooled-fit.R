bundled_trials <- function() {
  path <- system.file("extdata", "pooling-example.csv", package = "singlehit")
  raw <- readr::read_csv(path, show_col_types = FALSE)
  split(raw, raw$study_id)
}

test_that("pooled fit reproduces the pooled deviance reported by poolability_test", {
  datasets <- list(ward_fixture(), ward_fixture(), ward_fixture())
  pooled <- fit_pooled_dose_response(datasets)
  pooling <- poolability_test(datasets)

  expect_s3_class(pooled, "qdr_model_set")
  expect_named(pooled, c("exponential", "beta_poisson"))
  for (model in names(pooled)) {
    expect_equal(pooled[[model]]$deviance, pooling$deviance_pooled[pooling$model == model])
  }
})

test_that("pooled fit matches poolability_test for datasets that differ", {
  trials <- bundled_trials()[c("A", "B", "C")]
  pooled <- fit_pooled_dose_response(trials)
  pooling <- suppressWarnings(poolability_test(trials))

  expect_identical(names(pooled), pooling$model)
  for (model in names(pooled)) {
    expect_equal(pooled[[model]]$deviance, pooling$deviance_pooled[pooling$model == model])
  }
})

test_that("pooled fit stacks rows instead of merging shared doses", {
  trials <- bundled_trials()[c("A", "B")]
  fit <- fit_pooled_dose_response(trials, models = "exponential")$exponential

  # A and B share four doses, so stacking keeps eight observations, not four.
  expect_equal(nrow(fit$data), 8L)
  expect_equal(sum(duplicated(fit$data$dose)), 4L)
  expect_equal(sum(fit$data$total), sum(vapply(trials, function(d) sum(d$positive + d$negative), numeric(1))))
})

test_that("pooled fits work with the existing tidy, glance, predict, and plot methods", {
  pooled <- fit_pooled_dose_response(bundled_trials()[c("A", "B")])

  for (fit in pooled) {
    expect_s3_class(fit, "qdr_fit")
    expect_equal(nrow(tidy.qdr_fit(fit)), length(fit$coefficients))
    expect_equal(glance.qdr_fit(fit)$nobs, sum(fit$data$total))
    expect_length(predict(fit, c(1, 10)), 2L)
    expect_s3_class(ggplot2::autoplot(fit), "ggplot")
  }
})

test_that("bootstrapping a pooled fit refits stacked rows without merging shared doses", {
  fit <- fit_pooled_dose_response(bundled_trials()[c("A", "B")], models = "exponential")$exponential
  observed <- tibble::tibble(dose = fit$data$dose, positive = fit$data$positive, negative = fit$data$negative)

  refit <- bootstrap_refit(fit, observed, 1L)
  expect_true(refit$converged)
  expect_true(is.na(refit$error))
  expect_equal(refit$deviance, fit$deviance, tolerance = 1e-6)
  # Merging the shared doses would fit a different likelihood, so this is not a no-op.
  merged <- fit_dose_response(observed, "exponential")
  expect_false(isTRUE(all.equal(merged$deviance, fit$deviance)))

  boot <- bootstrap_dose_response(fit, times = 20, seed = 11)
  expect_s3_class(boot, "qdr_bootstrap")
  expect_true(all(boot$converged))
  expect_true(all(is.na(boot$error)))
})

test_that("single-dataset bootstrap replicates are unchanged by the stacked refit path", {
  # Values recorded before refit_sample() replaced the as_dose_response() refit.
  fit <- fit_dose_response(ward_fixture(), "exponential")
  boot <- bootstrap_dose_response(fit, times = 50, resample = "observed", seed = 7)

  expect_equal(boot$deviance[1:3], c(70.076486535651313, 148.151488878985504, 135.366519669797839), tolerance = 1e-6)
  expect_equal(boot$k[1:3], c(0.1865139951738617741, 0.0011582610496987319, 0.0011130731433597481), tolerance = 1e-6)
  expect_equal(boot$log_lik[1:2], c(-38.527689345641576, -78.868483778453253), tolerance = 1e-6)
})

test_that("fit_pooled_dose_response validates its inputs", {
  expect_error(fit_pooled_dose_response(ward_fixture()), "non-empty list")
  expect_error(fit_pooled_dose_response(list()), "non-empty list")
  expect_error(fit_pooled_dose_response(list(ward_fixture()), models = "invalid"), "non-empty subset")
  bad <- data.frame(dose = c(1, 10, 100), pos = c(0, 0, 1), neg = c(5, 5, 4))
  expect_error(fit_pooled_dose_response(list(good = ward_fixture(), bad = bad)), "bad")
})

test_that("fit_pooled_dose_response refuses mixed dose units", {
  a <- ward_fixture()
  b <- ward_fixture()
  a$dose_unit <- "FFU"
  b$dose_unit <- "PFU"
  expect_error(fit_pooled_dose_response(list(a = a, b = b), models = "exponential"), "different dose units")
  b$dose_unit <- "FFU"
  pooled <- fit_pooled_dose_response(list(a = a, b = b), models = "exponential")
  expect_equal(unique(pooled$exponential$data$dose_unit), "FFU")
})
