test_that("the plan gives each model the right budget and seed offset", {
  analysis <- example_analysis(c("exponential", "beta_poisson"))
  plan <- bootstrap_plan(analysis, times = 500, exact_times = 300, seed = 10)
  expect_named(plan, c("exponential", "beta_poisson"))
  expect_equal(vapply(plan, function(p) p$times, integer(1)), c(exponential = 500L, beta_poisson = 500L))
  # Same offsets as analyze_dose_response(): seed + index - 1.
  expect_equal(vapply(plan, function(p) p$seed, integer(1)), c(exponential = 10L, beta_poisson = 11L))
})

test_that("the exact model uses its own budget, and is ignored when not fitted", {
  with_exact <- example_analysis(c("exponential", "exact_beta_poisson"))
  plan <- bootstrap_plan(with_exact, times = 1000, exact_times = 200, seed = NULL)
  expect_equal(plan$exponential$times, 1000L)
  expect_equal(plan$exact_beta_poisson$times, 200L)
  expect_null(plan$exact_beta_poisson$seed)

  # A missing exact input falls back to the default budget.
  expect_equal(bootstrap_plan(with_exact, 1000, NULL)$exact_beta_poisson$times, bootstrap_defaults$exact_times)

  # Not fitted: a bad exact value is irrelevant and not validated.
  expect_no_error(bootstrap_plan(example_analysis(), times = 1000, exact_times = 99999))
})

test_that("a blank seed means no seed", {
  analysis <- example_analysis()
  expect_null(bootstrap_plan(analysis, 1000, NULL, NA)$exponential$seed)
  expect_null(bootstrap_plan(analysis, 1000, NULL, NULL)$exponential$seed)
})

test_that("invalid settings are refused with a clear message", {
  analysis <- example_analysis()
  expect_error(bootstrap_plan(analysis, 50), "between 100 and 10,000")
  expect_error(bootstrap_plan(analysis, 20000), "between 100 and 10,000")
  expect_no_error(bootstrap_plan(analysis, 10000))
  expect_error(bootstrap_plan(analysis, 1000.5), "whole number")
  expect_error(bootstrap_plan(analysis, NA_real_), "whole number")
  expect_error(bootstrap_plan(analysis, c(100, 200)), "whole number")
  expect_error(bootstrap_plan(analysis, 1000, seed = -1), "seed")
  expect_error(bootstrap_plan(analysis, 1000, seed = 1.5), "seed")
  expect_error(bootstrap_plan(analysis, 1000, seed = .Machine$integer.max), "seed")

  with_exact <- example_analysis(c("exponential", "exact_beta_poisson"))
  expect_error(bootstrap_plan(with_exact, 1000, exact_times = 5000), "between 100 and 1,000")
  expect_error(bootstrap_plan(with_exact, 1000, exact_times = 10), "Exact-model replicates")
})

test_that("progress is summarized per model and overall", {
  progress <- list(
    exponential = list(done = 50L, total = 50L, finished = TRUE),
    exact_beta_poisson = list(done = 1L, total = 10L, finished = FALSE)
  )
  summary <- progress_summary(progress)
  expect_equal(summary$models$model, c("exponential", "exact_beta_poisson"))
  expect_equal(summary$models$fraction, c(1, 0.1))
  expect_equal(summary$fraction, 51 / 60)
  expect_false(summary$finished)
  expect_true(progress_summary(list(a = list(done = 3L, total = 3L, finished = TRUE)))$finished)
})

test_that("bootstraps with fewer than two converged replicates are not attached", {
  make <- function(converged) structure(list(converged = converged), class = "data.frame", row.names = seq_along(converged))
  boots <- list(exponential = make(c(TRUE, TRUE, FALSE)), exact_beta_poisson = make(c(TRUE, FALSE, FALSE)))
  split <- attachable_bootstraps(boots)
  expect_equal(names(split$attached), "exponential")
  expect_equal(split$dropped, "exact_beta_poisson")
})

test_that("the interval and convergence tables describe a real bootstrap", {
  analysis <- example_analysis()
  boots <- singlehit::bootstrap_dose_response_models(analysis$fits, times = 100, seed = 1, backend = "sequential")
  tables <- bootstrap_tables(boots)
  expect_true(all(c("Model", "Quantity", "95% interval", "99% interval") %in% names(tables)))
  expect_setequal(tables$Model, c("Exponential", "Beta-Poisson"))
  expect_true("N50" %in% tables$Quantity)
  expect_true(all(grepl("^\\(.+, .+\\)$", tables[["95% interval"]])))

  convergence <- convergence_table(boots)
  expect_equal(convergence$Model, c("Exponential", "Beta-Poisson"))
  expect_match(convergence[["Converged replicates"]], "^\\d+ of 100$")
})

test_that("bands are attached only to the analysis they were computed from", {
  analysis <- example_analysis()
  boots <- singlehit::bootstrap_dose_response_models(analysis$fits, times = 100, seed = 1, backend = "sequential")
  other <- example_analysis(c("exponential", "exact_beta_poisson"))

  attached <- attach_bands(analysis, boots, analysis)
  expect_setequal(names(attached$bootstraps), c("exponential", "beta_poisson"))

  # A different analysis (or none) must never receive another fit's bands.
  expect_length(attach_bands(other, boots, analysis)$bootstraps, 0L)
  expect_null(attach_bands(NULL, boots, analysis))
  expect_identical(attach_bands(analysis, list(), analysis), analysis)
  expect_identical(attach_bands(analysis, boots, NULL), analysis)
})
