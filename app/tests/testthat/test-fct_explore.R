analysis_with_bands <- function() {
  analysis <- example_analysis()
  boots <- singlehit::bootstrap_dose_response_models(analysis$fits, times = 20, seed = 1, backend = "sequential")
  singlehit::add_bootstrap(analysis, boots)
}

test_that("explore_datasets returns the three named data frames with identifier names", {
  datasets <- explore_datasets(example_analysis())
  expect_named(datasets, c("observed", "curves", "model_residuals"))
  expect_setequal(unname(explore_dataset_choices), names(datasets))
  expect_true(all(vapply(datasets, is.data.frame, logical(1))))
  expect_true(all(make.names(names(datasets)) == names(datasets)))
})

test_that("curves carry display model labels and NA bands without a bootstrap", {
  curves <- explore_datasets(example_analysis())$curves
  expect_setequal(curves$model, c("Exponential", "Beta-Poisson"))
  expect_true(all(c("dose", "estimate", "level", "lower", "upper") %in% names(curves)))
  expect_true(all(is.na(curves$lower)))
  expect_true(is.factor(curves$level))
  expect_length(levels(curves$level), 0L)
})

test_that("an attached bootstrap adds bands and one row per dose and confidence level", {
  curves <- explore_datasets(analysis_with_bands())$curves
  expect_false(anyNA(curves$lower))
  expect_s3_class(curves$level, "factor")
  expect_equal(levels(curves$level), c("95%", "99%"))
  per_dose <- table(curves$model, curves$dose)
  expect_true(all(per_dose == 2L))
})

test_that("a bootstrap that cannot give bands falls back to the bare curve", {
  analysis <- analysis_with_bands()
  analysis$bootstraps$exponential$converged <- FALSE
  # Confirm the fallback is reached through singlehit's own replicate-count error.
  expect_error(
    singlehit::prediction_curve(analysis$fits$exponential, bootstrap = analysis$bootstraps$exponential),
    "converged bootstrap replicates"
  )
  curves <- explore_datasets(analysis)$curves
  expect_true(all(is.na(curves$lower[curves$model == "Exponential"])))
  expect_false(anyNA(curves$lower[curves$model == "Beta-Poisson"]))
})

test_that("residuals pair each model with fitted values and deviance residuals", {
  residuals <- explore_datasets(example_analysis())$model_residuals
  expect_setequal(residuals$model, c("Exponential", "Beta-Poisson"))
  expect_true(all(c("dose", "response", ".fitted", ".resid") %in% names(residuals)))
  expect_equal(nrow(residuals), 6L)
})
