test_that("labels fall back to the raw value for unknown codes", {
  expect_equal(model_label_ui(c("exponential", "mystery")), c("Exponential", "mystery"))
  expect_equal(recommendation_label(c("recommended", "other")), c("Recommended", "other"))
  expect_equal(agreement_label("no_consensus"), "No consensus")
})

test_that("p-values and numbers format missing values as n/a", {
  expect_equal(format_p(c(0.0004, 0.5, NA)), c("<0.001", "0.5", "n/a"))
  expect_equal(format_number(c(1.23456789, NA)), c("1.235", "n/a"))
})

test_that("with_captured_warnings returns warnings without printing them", {
  out <- with_captured_warnings({
    warning("first")
    warning("first")
    42
  })
  expect_equal(out$value, 42)
  expect_equal(out$warnings, "first")
  expect_null(out$error)
})

test_that("with_captured_warnings keeps warnings raised before an error", {
  out <- with_captured_warnings({
    warning("before")
    stop("boom")
  })
  expect_null(out$value)
  expect_equal(out$error, "boom")
  expect_equal(out$warnings, "before")
})

test_that("the fast analysis builds display tables for every fitted model", {
  run <- run_fast_analysis(example_data(), c("exponential", "beta_poisson"), alpha = 0.05)
  expect_null(run$error)
  expect_s3_class(run$value$analysis, "qdr_analysis")
  expect_setequal(run$value$parameters$Model, c("Exponential", "Beta-Poisson"))
  expect_equal(nrow(run$value$fit_stats), 2L)
  expect_true(all(c("Deviance", "Goodness-of-fit p", "AIC", "BIC") %in% names(run$value$fit_stats)))
  expect_length(run$value$analysis$bootstraps, 0L)
})

test_that("the example data recommends the exponential model but the experimental consensus picks beta-Poisson", {
  analysis <- example_analysis()
  expect_equal(headline_decision(analysis$assessment)$model, "Exponential")
  consensus <- singlehit::consensus_model_decision(analysis)
  expect_equal(consensus$model[consensus$consensus_selected], "beta_poisson")
})

test_that("headline_decision reports when no model is recommended", {
  assessment <- data.frame(
    model = c("exponential", "beta_poisson"),
    recommendation = c("preferred_but_inadequate", "not_recommended"),
    conclusion = c("Exponential is inadequate.", "Beta-Poisson is not preferred."),
    stringsAsFactors = FALSE
  )
  headline <- headline_decision(assessment)
  expect_equal(headline$status, "none")
  expect_true(is.na(headline$model))
  expect_match(headline$text, "No model is recommended")
  expect_match(headline$text, "Exponential is inadequate")
})

test_that("assessment_table tolerates missing deviance comparisons", {
  assessment <- data.frame(
    model = c("beta_poisson", "exact_beta_poisson"),
    deviance = c(1.2, 1.1),
    gof_p_value = c(0.4, NA),
    appropriate = c(TRUE, TRUE),
    deviance_difference = c(NA_real_, NA_real_),
    chi_square_p_value = c(NA_real_, NA_real_),
    preferred = c(TRUE, FALSE),
    recommendation = c("recommended", "acceptable_alternative")
  )
  table <- assessment_table(assessment)
  expect_equal(table[["Deviance difference"]], c("n/a", "n/a"))
  expect_equal(table[["Comparison p"]], c("n/a", "n/a"))
})

test_that("the assessment note distinguishes the exponential baseline from a missing exponential", {
  baseline <- data.frame(model = c("exponential", "beta_poisson"), deviance_difference = c(NA, 3.7))
  expect_match(assessment_note(baseline), "baseline for the deviance comparison")
  expect_false(grepl("not among", assessment_note(baseline)))

  without <- data.frame(model = c("beta_poisson", "exact_beta_poisson"), deviance_difference = c(NA_real_, NA_real_))
  expect_match(assessment_note(without), "not among the fitted models")

  complete <- data.frame(model = c("a", "b"), deviance_difference = c(1, 2))
  expect_null(assessment_note(complete))
})

test_that("data_preview shows whole-number counts", {
  preview <- data_preview(example_data())
  expect_equal(preview$positive, c("1", "5", "9"))
  expect_equal(preview$total, c("10", "10", "10"))
  expect_equal(preview$dose, c("1", "10", "100"))
})
