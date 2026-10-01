test_that("bootstrap results are reproducible and tidy", {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  first <- bootstrap_dose_response(fit, times = 20, seed = 42)
  second <- bootstrap_dose_response(fit, times = 20, seed = 42)
  intervals <- bootstrap_confint(first)

  expect_s3_class(first, "qdr_bootstrap")
  expect_identical(attr(first, "backend"), "sequential")
  expect_equal(tibble::as_tibble(first), tibble::as_tibble(second))
  expect_equal(nrow(first), 20L)
  expect_true(all(first$converged))
  expect_setequal(intervals$term, c("alpha", "n50", "ed10", "ed50"))
  expect_true(all(intervals$lower <= intervals$upper))
})

test_that("mirai and sequential backends use identical bootstrap samples", {
  skip_if_not_installed("mirai", minimum_version = "2.5.0")
  skip_if(Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "", "requires an installed package namespace")
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  sequential <- bootstrap_dose_response(fit, times = 21, seed = 42)

  mirai::daemons(1, dispatcher = FALSE)
  on.exit(
    {
      mirai::daemons(0)
      Sys.sleep(1)
    },
    add = TRUE
  )
  parallel <- bootstrap_dose_response(fit, times = 21, seed = 42, backend = "mirai")

  expect_identical(attr(parallel, "backend"), "mirai")
  expect_equal(lapply(parallel, identity), lapply(sequential, identity))
})

test_that("automatic backend and worker defaults are conservative", {
  expect_identical(select_bootstrap_backend("auto", 1000L, TRUE), "sequential")
  expect_identical(select_bootstrap_backend("auto", 1001L, TRUE), "mirai")
  expect_identical(select_bootstrap_backend("auto", 1001L, FALSE), "sequential")
  expect_identical(recommended_bootstrap_workers(8L), 6L)
  expect_identical(recommended_bootstrap_workers(24L), 18L)
  expect_identical(recommended_bootstrap_workers(1L), 1L)
})

test_that("async bootstrap returns a collectable job and cleans up its daemons", {
  skip_if_not_installed("mirai", minimum_version = "2.5.0")
  skip_if(Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "", "requires an installed package namespace")
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  job <- bootstrap_dose_response_async(fit, times = 8, seed = 42, workers = 1)

  expect_s3_class(job, "qdr_bootstrap_job")
  expect_true(mirai::daemons_set(.compute = job$compute))

  result <- collect_bootstrap(job)

  expect_s3_class(result, "qdr_bootstrap")
  expect_equal(nrow(result), 8L)
  expect_false(mirai::daemons_set(.compute = job$compute))
})

test_that("prediction curves and plots include bootstrap uncertainty", {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  bootstrap <- bootstrap_dose_response(fit, times = 20, seed = 42)
  curve <- prediction_curve(fit, bootstrap, levels = c(0.95, 0.99), points = 30)

  expect_equal(nrow(curve), 60L)
  expect_setequal(curve$level, c(0.95, 0.99))
  expect_true(all(curve$lower <= curve$upper))
  model_plot <- plot_dose_response(fit, bootstrap)
  bootstrap_plot <- ggplot2::autoplot(bootstrap)
  expect_s3_class(model_plot, "ggplot")
  expect_s3_class(bootstrap_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(model_plot))
  expect_no_error(ggplot2::ggplot_build(bootstrap_plot))
})

test_that("the complete analysis workflow returns plot-ready results", {
  analysis <- analyze_dose_response(ward_fixture(), bootstrap_times = 10, seed = 2026)

  expect_s3_class(analysis, "qdr_analysis")
  expect_named(analysis, c("data", "trend", "fits", "goodness_of_fit", "comparison", "assessment", "bootstraps"))
  expect_named(analysis$fits, c("exponential", "beta_poisson"))
  expect_named(analysis$bootstraps, c("exponential", "beta_poisson"))
  expect_equal(analysis$assessment, assess_dose_response_models(analysis))
  analysis_plot <- ggplot2::autoplot(analysis, points = 30)
  expect_s3_class(analysis_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(analysis_plot))
  overlay <- plot_model_overlay(analysis, points = 30)
  expect_s3_class(overlay, "ggplot")
  expect_length(overlay$layers, 2L)
  expect_equal(nrow(ggplot2::ggplot_build(overlay)$data[[2L]]), 60L)
})

test_that("prediction curve builds bootstrap bands for the exact model", {
  fit <- fit_dose_response(ward_fixture(), "exact_beta_poisson")
  boot <- bootstrap_dose_response(fit, times = 15, seed = 3)
  curve <- prediction_curve(fit, boot, points = 10)

  expect_true(all(curve$lower <= curve$upper))
  expect_true(all(is.finite(curve$estimate)))
  exact_plot <- ggplot2::autoplot(boot)
  expect_s3_class(exact_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(exact_plot))
})

test_that("three-model analysis fits, bootstraps, and plots the exact model", {
  analysis <- analyze_dose_response(
    ward_fixture(),
    models = c("exponential", "beta_poisson", "exact_beta_poisson"),
    bootstrap_times = 12,
    exact_bootstrap_times = 12,
    seed = 2026
  )

  expect_named(analysis$fits, c("exponential", "beta_poisson", "exact_beta_poisson"))
  expect_named(analysis$bootstraps, c("exponential", "beta_poisson", "exact_beta_poisson"))
  expect_equal(nrow(analysis$assessment), 3L)
  analysis_plot <- ggplot2::autoplot(analysis, points = 20)
  expect_s3_class(analysis_plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(analysis_plot))
})

test_that("the exact model is fit and compared without bootstrapping when opted out", {
  analysis <- analyze_dose_response(
    ward_fixture(),
    models = c("exponential", "beta_poisson", "exact_beta_poisson"),
    bootstrap_times = 8,
    exact_bootstrap_times = 0,
    seed = 1
  )

  expect_true("exact_beta_poisson" %in% analysis$comparison$model)
  expect_named(analysis$bootstraps, c("exponential", "beta_poisson"))
})

test_that("model set and bootstrap counts are validated", {
  expect_error(fit_dose_response_models(ward_fixture(), models = "nonsense"), "subset of")
  expect_error(fit_dose_response_models(ward_fixture(), models = character(0)), "subset of")
  expect_error(analyze_dose_response(ward_fixture(), exact_bootstrap_times = -1), "exact_bootstrap_times")
  expect_error(bootstrap_dose_response(fit_dose_response(ward_fixture()), workers = 0), "workers")
  expect_error(collect_bootstrap(list()), "qdr_bootstrap_job")
})
