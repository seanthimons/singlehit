has_ribbon <- function(plot) {
  any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))
}

test_that("add_bootstrap attaches results so the analysis plot draws bands", {
  analysis <- analyze_dose_response(ward_fixture(), bootstrap_times = 0, check_trend = FALSE)
  expect_length(analysis$bootstraps, 0L)
  expect_false(has_ribbon(ggplot2::autoplot(analysis, points = 30)))

  boots <- bootstrap_dose_response_models(analysis$fits, times = 20, seed = 2026)
  updated <- add_bootstrap(analysis, boots)

  expect_s3_class(updated, "qdr_analysis")
  expect_named(updated$bootstraps, c("exponential", "beta_poisson"))
  expect_identical(updated$bootstraps$exponential, boots$exponential)
  expect_identical(updated$fits, analysis$fits)
  expect_length(analysis$bootstraps, 0L) # the input is not modified
  expect_true(has_ribbon(ggplot2::autoplot(updated, points = 30)))
  expect_no_error(ggplot2::ggplot_build(ggplot2::autoplot(updated, points = 30)))
})

test_that("add_bootstrap merges a partial set and keeps other models unchanged", {
  analysis <- analyze_dose_response(ward_fixture(), bootstrap_times = 0, check_trend = FALSE)
  boots <- bootstrap_dose_response_models(analysis$fits, times = 10, seed = 5)

  partial <- add_bootstrap(analysis, boots["exponential"])
  expect_named(partial$bootstraps, "exponential")
  expect_true(has_ribbon(ggplot2::autoplot(partial, points = 30)))

  both <- add_bootstrap(partial, boots["beta_poisson"])
  expect_setequal(names(both$bootstraps), c("exponential", "beta_poisson"))
})

test_that("add_bootstrap rejects inputs that do not match the analysis", {
  analysis <- analyze_dose_response(ward_fixture(), bootstrap_times = 0, check_trend = FALSE)
  boots <- bootstrap_dose_response_models(analysis$fits, times = 5, seed = 1)
  other <- fit_dose_response(
    data.frame(dose = c(1, 3, 10, 30), positive = c(0, 0, 1, 3), negative = c(10, 10, 9, 7)),
    "exponential"
  )
  other_boot <- bootstrap_dose_response(other, times = 5, seed = 1)

  expect_error(add_bootstrap(analysis$fits, boots), "qdr_analysis")
  expect_error(add_bootstrap(analysis, list()), "non-empty named list")
  expect_error(add_bootstrap(analysis, list(boots$exponential)), "non-empty named list")
  expect_error(add_bootstrap(analysis, boots$exponential), "non-empty named list")
  expect_error(
    add_bootstrap(analysis, list(exponential = boots$exponential, exponential = boots$exponential)),
    "non-empty named list"
  )
  expect_error(add_bootstrap(analysis, setNames(list(boots$exponential), "")), "non-empty named list")
  expect_error(add_bootstrap(analysis, list(exponential = analysis$fits$exponential)), "must be a qdr_bootstrap")
  expect_error(add_bootstrap(analysis, list(gamma = boots$exponential)), "No fitted model named gamma")
  expect_error(
    add_bootstrap(analysis, list(beta_poisson = boots$exponential)),
    "not computed from this analysis's fit"
  )
  expect_error(
    add_bootstrap(analysis, list(exponential = other_boot)),
    "not computed from this analysis's fit"
  )
  # same data, different coefficients: the bootstrap belongs to a different fit
  shifted <- analysis$fits$exponential
  shifted$coefficients <- shifted$coefficients * 2
  expect_error(
    add_bootstrap(analysis, list(exponential = bootstrap_dose_response(shifted, times = 5, seed = 1))),
    "not computed from this analysis's fit"
  )
})
