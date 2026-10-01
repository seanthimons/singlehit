test_that("identical datasets are perfectly poolable", {
  result <- poolability_test(list(a = ward_fixture(), b = ward_fixture()))

  expect_setequal(result$model, c("exponential", "beta_poisson"))
  expect_true(all(result$lrt_statistic < 1e-4))
  expect_true(all(result$poolable))
  expect_true(all(result$p_value > 0.99))
  expect_equal(result$df[result$model == "exponential"], 1)
  expect_equal(result$df[result$model == "beta_poisson"], 2)
  # the pooled and unpooled deviances match when the data are identical
  expect_equal(result$deviance_pooled, result$deviance_unpooled, tolerance = 1e-4)
})

test_that("clearly different dose-response curves are not poolable", {
  steep <- data.frame(dose = c(1, 3, 10, 30), pos = c(2, 6, 9, 10), neg = c(8, 4, 1, 0))
  shallow <- data.frame(dose = c(1, 3, 10, 30), pos = c(0, 0, 1, 3), neg = c(10, 10, 9, 7))
  result <- suppressWarnings(poolability_test(list(steep = steep, shallow = shallow)))

  expect_true(all(!result$poolable))
  expect_true(all(result$p_value < 0.05))
})

test_that("poolability test reports correct df and deviances for three datasets", {
  datasets <- list(ward_fixture(), ward_fixture(), ward_fixture())
  result <- poolability_test(datasets)

  expect_equal(result$n_datasets, c(3L, 3L))
  expect_equal(result$df[result$model == "exponential"], 2) # k(m-1) = 1*2
  expect_equal(result$df[result$model == "beta_poisson"], 4) # k(m-1) = 2*2
  # unpooled deviance is the sum of the individual fits
  individual <- sum(vapply(
    datasets,
    function(d) fit_dose_response(d, "beta_poisson")$deviance,
    numeric(1)
  ))
  expect_equal(
    result$deviance_unpooled[result$model == "beta_poisson"],
    individual,
    tolerance = 1e-4
  )
})

test_that("stacked pooling reproduces the reference layout, not aggregation", {
  # QMRA-wiki pooled experiment "253, 254" (Naegleria fowleri): stacked, the two
  # sub-studies share doses. The reference exponential deviance is ~8.85;
  # aggregating (summing same-dose rows) would wrongly give ~3.05.
  e253 <- data.frame(dose = c(2.5e6, 5e6, 1e7), pos = c(4, 19, 10), neg = c(6, 1, 0))
  e254 <- data.frame(dose = c(1e6, 2.5e6, 5e6, 1e7), pos = c(4, 12, 14, 20), neg = c(16, 8, 6, 0))
  result <- poolability_test(list(e253 = e253, e254 = e254), models = "exponential")

  expect_equal(result$deviance_pooled, 8.85, tolerance = 0.05)
  expect_gt(result$deviance_pooled, 6) # unambiguously the stacked, not aggregated, value
})

test_that("group_datasets clusters poolable datasets and isolates distinct ones", {
  odd <- data.frame(dose = c(1, 3, 10, 30), pos = c(0, 0, 1, 3), neg = c(10, 10, 9, 7))
  datasets <- list(w1 = ward_fixture(), w2 = ward_fixture(), odd = odd)

  agglomerative <- suppressWarnings(group_datasets(datasets, models = "beta_poisson"))
  exhaustive <- suppressWarnings(
    group_datasets(datasets, models = "beta_poisson", method = "exhaustive")
  )

  expect_named(agglomerative, c("dataset", "model", "group"))
  # the two identical wards share a group; the odd dataset is separate
  group_of <- function(g, name) g$group[g$dataset == name]
  expect_equal(group_of(agglomerative, "w1"), group_of(agglomerative, "w2"))
  expect_true(group_of(agglomerative, "odd") != group_of(agglomerative, "w1"))
  # both methods agree at m = 3
  expect_equal(agglomerative$group, exhaustive$group)
})

test_that("pooling validates its inputs", {
  # need at least two datasets to test pooling
  expect_error(poolability_test(list(ward_fixture())), "at least two")
  # a screen-failing dataset errors with its name
  bad <- data.frame(dose = c(1, 10, 100), pos = c(0, 0, 1), neg = c(5, 5, 4)) # one responder
  expect_error(
    poolability_test(list(good = ward_fixture(), bad = bad)),
    "bad"
  )
  # a single dataset groups trivially
  grouped <- group_datasets(list(only = ward_fixture()))
  expect_true(all(grouped$group == 1L))
})

test_that("combination reporting covers every subset and preserves pooling results", {
  trials <- list(A = ward_fixture(), B = ward_fixture(), C = ward_fixture())
  result <- poolability_combinations(trials)
  expect_equal(nrow(result), 8L)
  expect_equal(unique(result$combination), c("A + B", "A + C", "B + C", "A + B + C"))
  expect_equal(result$datasets[[1]], c("A", "B"))
  expect_equal(result$datasets[[7]], c("A", "B", "C"))
  expect_true(all(result$poolable))
  expect_true(all(result$converged))
  expect_equal(result[result$n_datasets == 3, names(poolability_test(trials))], poolability_test(trials))

  odd <- data.frame(dose = c(1, 3, 10, 30), pos = c(0, 0, 1, 3), neg = c(10, 10, 9, 7))
  mixed <- suppressWarnings(poolability_combinations(list(A = ward_fixture(), B = ward_fixture(), C = odd), models = "exponential"))
  expect_true(mixed$poolable[mixed$combination == "A + B"])
  expect_false(any(mixed$poolable[mixed$combination != "A + B"]))
  expect_error(poolability_combinations(trials[1]), "between two")
  expect_error(poolability_combinations(rep(trials, 3)), "got 9")
  expect_error(poolability_combinations(trials, max_datasets = NA_real_), "whole number")
  expect_error(poolability_combinations(trials, max_datasets = 2.5), "whole number")
  expect_error(poolability_combinations(trials, models = "invalid"), "non-empty subset")
  expect_error(poolability_combinations(trials, alpha = 1), "between zero and one")
})


test_that("bundled synthetic collection demonstrates passing and failing combinations", {
  path <- system.file("extdata", "pooling-example.csv", package = "singlehit")
  raw <- readr::read_csv(path, show_col_types = FALSE)
  trials <- split(raw, raw$study_id)
  report <- poolability_combinations(trials)
  expect_equal(nrow(report), 8L)
  expect_true(all(report$converged))
  expect_true(all(report$poolable[report$combination == "A + B"]))
  expect_false(any(report$poolable[report$combination != "A + B"]))
  groups <- group_datasets(trials, method = "exhaustive")
  for (model in unique(groups$model)) {
    g <- groups[groups$model == model, ]
    expect_equal(g$group[g$dataset == "A"], g$group[g$dataset == "B"])
    expect_false(g$group[g$dataset == "A"] == g$group[g$dataset == "C"])
  }
})
