has_ribbon <- function(plot) {
  any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))
}

analysis_with_bootstrap <- function() {
  analysis <- example_analysis()
  boots <- singlehit::bootstrap_dose_response_models(analysis$fits, times = 20, seed = 1, backend = "sequential")
  singlehit::add_bootstrap(analysis, boots)
}

test_that("each view returns a ggplot", {
  analysis <- example_analysis()
  expect_s3_class(build_figure(analysis, "overlay"), "ggplot")
  expect_s3_class(build_figure(analysis, "panels"), "ggplot")
  expect_s3_class(build_figure(analysis, "single", "beta_poisson"), "ggplot")
})

test_that("the single view refuses a model that was not fitted", {
  analysis <- example_analysis()
  expect_error(build_figure(analysis, "single", "exact_beta_poisson"), "fitted models")
  expect_error(build_figure(analysis, "single", NULL), "fitted models")
})

test_that("without a bootstrap no view draws confidence bands", {
  analysis <- example_analysis()
  expect_false(has_ribbon(build_figure(analysis, "panels")))
  expect_false(has_ribbon(build_figure(analysis, "single", "exponential")))
})

test_that("an attached bootstrap adds bands to the panel and single views but not the overlay", {
  analysis <- analysis_with_bootstrap()
  expect_true(has_ribbon(build_figure(analysis, "panels")))
  expect_true(has_ribbon(build_figure(analysis, "single", "exponential")))
  expect_false(has_ribbon(build_figure(analysis, "overlay")))
})

test_that("model choices carry display labels", {
  expect_equal(
    model_choices(example_analysis()),
    c(Exponential = "exponential", "Beta-Poisson" = "beta_poisson")
  )
})

test_that("bounded_number clamps and falls back", {
  expect_equal(bounded_number(5, 2, 20, 7), 5)
  expect_equal(bounded_number(100, 2, 20, 7), 20)
  expect_equal(bounded_number(-1, 2, 20, 7), 2)
  expect_equal(bounded_number(NA_real_, 2, 20, 7), 7)
  expect_equal(bounded_number(NULL, 2, 20, 7), 7)
})
