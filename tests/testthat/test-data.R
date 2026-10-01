test_that("Ward data is standardized from legacy column names", {
  ward <- ward_fixture()

  expect_s3_class(ward, "tbl_df")
  expect_named(ward, c("dose", "positive", "negative", "total", "response"))
  expect_equal(nrow(ward), 8L)
  expect_equal(sum(ward$total), 59)
  expect_true(all(diff(ward$dose) > 0))
  expect_equal(ward$response, ward$positive / ward$total)
})

test_that("duplicate doses are combined", {
  input <- data.frame(
    dose = c(1, 1, 10, 100),
    pos = c(1, 2, 4, 3),
    neg = c(3, 2, 0, 1)
  )
  result <- as_dose_response(input)

  expect_equal(nrow(result), 3L)
  expect_equal(result$positive[[1L]], 3)
  expect_equal(result$negative[[1L]], 5)
})

test_that("fewer than three distinct dose groups fail after combining", {
  # three raw rows collapsing to two distinct doses is not enough
  expect_error(
    as_dose_response(data.frame(dose = c(1, 1, 10), pos = c(1, 1, 4), neg = c(3, 3, 1))),
    "three distinct dose groups"
  )
})

test_that("more than one positive response is required", {
  # a single lone responder cannot identify the model
  expect_error(
    as_dose_response(data.frame(dose = c(1, 10, 100), pos = c(0, 0, 1), neg = c(5, 5, 4))),
    "more than one positive response"
  )
  # no responders at all
  expect_error(
    as_dose_response(data.frame(dose = c(1, 10, 100), pos = c(0, 0, 0), neg = c(5, 5, 5))),
    "more than one positive response"
  )
})

test_that("one responding dose with multiple responders is accepted (reading B)", {
  # mirrors CAMRA-fit experiments (e.g. Enterovirus 62): a single responding
  # dose but two responders total meets the criteria and fits
  result <- as_dose_response(data.frame(
    dose = c(100, 250, 1000),
    pos = c(0, 0, 2),
    neg = c(3, 6, 4)
  ))
  expect_equal(nrow(result), 3L)
  expect_equal(sum(result$positive), 2)
})

test_that("invalid grouped counts fail before fitting", {
  expect_error(
    as_dose_response(data.frame(dose = c(0, 1, 2), pos = 1, neg = 1)),
    "greater than zero"
  )
  expect_error(
    as_dose_response(data.frame(dose = 1:3, pos = c(1, 1.5, 1), neg = 1)),
    "whole numbers"
  )
  expect_error(
    as_dose_response(data.frame(dose = 1:3, cases = 1, controls = 1)),
    "detect the positive"
  )
})


test_that("optional experiment metadata survives coercion, import, and fitting", {
  input <- data.frame(
    dose = c(1, 1, 10, 100), pos = c(1, 2, 4, 3), neg = c(3, 2, 0, 1),
    study_id = "trial_a", host = "human", dose_unit = "FFU",
    endpoint = "infection", citation = "Example study",
    total = 999, response = 999
  )
  result <- as_dose_response(input)
  expect_equal(result$positive, c(3, 4, 3))
  expect_equal(result$total, c(8, 4, 4))
  expect_equal(result$response, result$positive / result$total)
  expect_equal(result$study_id, rep("trial_a", 3))
  expect_equal(result$citation, rep("Example study", 3))
  expect_identical(as_dose_response(result), result)
  expect_identical(fit_dose_response(result, "exponential")$data, result)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  readr::write_csv(input, path)
  expect_equal(read_dose_response(path), result)

  for (column in c("study_id", "host", "dose_unit", "endpoint")) {
    mixed <- input
    mixed[[column]][2] <- "different"
    expect_error(as_dose_response(mixed), column)
    mixed[[column]][2] <- NA_character_
    expect_error(as_dose_response(mixed), column)
    mixed[[column]][2] <- " "
    expect_error(as_dose_response(mixed), column)
  }
  input$citation[2] <- "Different study"
  expect_error(as_dose_response(input), "citation.*disagrees")
  input <- data.frame(dose = 1:3, pos = c(1, 2, 3), neg = 3, positive = "metadata")
  expect_error(as_dose_response(input, positive = "pos"), "reserved column names")
  example_path <- system.file("extdata", "dose-response-example.csv", package = "singlehit")
  example <- read_dose_response(example_path)
  expect_equal(example$study_id, rep("example_trial", 3))
  template_path <- system.file("extdata", "dose-response-template.csv", package = "singlehit")
  expect_identical(names(readr::read_csv(template_path, show_col_types = FALSE)),
                   c("study_id", "host", "dose_unit", "endpoint", "dose", "positive", "negative"))
})
