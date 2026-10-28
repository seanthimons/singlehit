sample_pool <- function(ids) {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  keys <- experiments$experiment_key[experiments$experiment_id %in% ids]
  list(rows = rows, experiments = experiments, selection = pool_selection(rows, experiments, keys))
}

failed <- function(checks) checks$id[!checks$passed]

test_that("two experiments in one unit pass every blocking check", {
  pool <- sample_pool(c("21", "23"))
  checks <- pool_check(pool$selection)
  expect_false(pool_blocked(checks))
  # The wiki pooled guinea pig with diabetic rat (different strains too);
  # these differences only warn.
  expect_equal(failed(checks), c("host", "strain"))
  expect_equal(unique(checks$severity[!checks$passed]), "warn")
})

test_that("data problems and dose units block the test", {
  pool <- sample_pool(c("21", "21,23", "71", "Ojielo2003"))
  checks <- pool_check(pool$selection)
  expect_true(pool_blocked(checks))
  expect_true(all(c("fittable", "wiki_pooled", "dose_unit") %in% failed(checks)))
  expect_true(all(c("outcome_group", "response_type", "route", "pathogen") %in% failed(checks)))
  expect_false("size" %in% failed(checks))
})

test_that("a single experiment, an unreported unit, or repeated counts block", {
  expect_true("size" %in% failed(pool_check(sample_pool("21")$selection)))

  pool <- sample_pool(c("21", "23"))
  unreported <- pool$selection
  unreported$dose_unit[[2L]] <- "Not reported"
  expect_match(pool_check(unreported)$message[pool_check(unreported)$id == "dose_unit"], "No dose unit is reported")

  copied <- pool$selection
  copied$signature[[2L]] <- copied$signature[[1L]]
  expect_true("duplicate" %in% failed(pool_check(copied)))

  repeated <- pool$selection
  repeated$experiment_id[[2L]] <- repeated$experiment_id[[1L]]
  expect_match(pool_check(repeated)$message[pool_check(repeated)$id == "duplicate"], "appears more than once")
})

test_that("labels keep experiments with the same id apart", {
  pool <- sample_pool("1")
  expect_setequal(pool$selection$label, c("Yersinia pestis #1", "Variola major #1"))
})

test_that("every rule has a known severity", {
  expect_true(all(vapply(pool_rules, function(rule) rule$severity %in% c("block", "warn"), logical(1L))))
  expect_false(anyDuplicated(vapply(pool_rules, `[[`, character(1L), "id")) > 0L)
})

test_that("pooling 21 and 23 reproduces the wiki's own pooled entry", {
  pool <- sample_pool(c("21", "23"))
  run <- pool_run(pool_datasets(pool$rows, pool$selection), c("exponential", "beta_poisson"))
  expect_false(inherits(run, "error"))
  beta <- generics::tidy(run$pooled$beta_poisson)
  # The wiki reports a = 2.13E-01, N50 = 4.77E+02 for "21,23".
  expect_equal(signif(beta$estimate[beta$term == "alpha"], 3), 0.213)
  expect_equal(signif(beta$estimate[beta$term == "n50"], 3), 477)
  expect_true(run$test$poolable[run$test$model == "beta_poisson"])
  expect_false(run$test$poolable[run$test$model == "exponential"])

  reference <- pool_wiki_reference(pool$rows, pool$experiments, pool$selection)
  expect_equal(reference$`Wiki entry`, "21,23")
  expect_match(reference$`Reported parameters`, "4.77E+02", fixed = TRUE)
})

test_that("the results table never says the experiments are poolable", {
  pool <- sample_pool(c("21", "23"))
  run <- pool_run(pool_datasets(pool$rows, pool$selection), c("exponential", "beta_poisson"))
  table <- pool_test_table(run)
  expect_equal(table$Result, c("Pooling rejected at alpha = 0.05", "Pooling not rejected at alpha = 0.05"))
  expect_false(any(grepl("are poolable", unlist(table))))
})

test_that("pooled datasets carry one label per experiment and one unit spelling", {
  pool <- sample_pool(c("21", "23"))
  datasets <- pool_datasets(pool$rows, pool$selection)
  expect_equal(names(datasets), pool$selection$short)
  expect_equal(unique(datasets[[2L]]$study_id), pool$selection$short[[2L]])
  expect_equal(unique(unlist(lapply(datasets, `[[`, "dose_unit"))), "CFU")
})

test_that("errors are returned, not thrown", {
  datasets <- list(
    a = data.frame(dose = c(1, 10, 100), positive = c(1, 5, 9), negative = c(9, 5, 1), dose_unit = "CFU"),
    b = data.frame(dose = c(1, 10, 100), positive = c(1, 5, 9), negative = c(9, 5, 1), dose_unit = "PFU")
  )
  expect_s3_class(pool_run(datasets, "exponential"), "error")
})

test_that("the plots build", {
  pool <- sample_pool(c("21", "23"))
  run <- pool_run(pool_datasets(pool$rows, pool$selection), c("exponential", "beta_poisson"))
  expect_no_error(ggplot2::ggplot_build(pool_plot(run)))
  expect_no_error(ggplot2::ggplot_build(pool_forest(run)))
  parameters <- pool_parameters(run)
  expect_setequal(unique(parameters$experiment), c(pool$selection$short, "Pooled"))
})

test_that("wiki-pooled ids are split on commas, ampersands, and 'and'", {
  expect_equal(pool_wiki_ids("281,282 (pooled)"), c("281", "282"))
  expect_equal(pool_wiki_ids("297 & 298"), c("297", "298"))
  expect_equal(pool_wiki_ids("300 and 301"), c("300", "301"))
  expect_equal(pool_wiki_ids("142, 143, 144"), c("142", "143", "144"))
})

test_that("log entries record the run, the rationale, and acknowledged warnings", {
  pool <- sample_pool(c("21", "23"))
  checks <- pool_check(pool$selection)
  run <- pool_run(pool_datasets(pool$rows, pool$selection), "beta_poisson")
  entry <- pool_log_entry(run, pool$selection, checks, "Same pathogen, route, and outcome.", "sample")
  expect_equal(nrow(entry), 1L)
  expect_equal(entry$rationale, "Same pathogen, route, and outcome.")
  expect_equal(entry$warnings_acknowledged, "One host; One strain or isolate")
  expect_equal(entry$dose_unit, "CFU")
  expect_false(entry$pooling_rejected)
})

test_that("a selection that matches nothing is empty, not an error", {
  rows <- read_qmra_workbook(qmra_sample_path())
  selection <- pool_selection(rows, library_experiments(rows), "not|a|real|key")
  expect_equal(nrow(selection), 0L)
  expect_true("size" %in% failed(pool_check(selection)))
})

crypto_pool <- function() {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  keys <- experiments$experiment_key[grepl("Cryptosporidium", experiments$pathogen)]
  list(rows = rows, experiments = experiments, selection = pool_selection(rows, experiments, keys))
}

test_that("different isolates warn, and short labels carry the isolate", {
  pool <- crypto_pool()
  checks <- pool_check(pool$selection)
  expect_false(pool_blocked(checks))
  expect_equal(failed(checks), "strain")
  expect_true("#140 (TAMU isolate)" %in% pool$selection$short)
})

test_that("exploration finds the isolate groups and logs how much it searched", {
  pool <- crypto_pool()
  explore <- pool_explore(pool_datasets(pool$rows, pool$selection), c("exponential", "beta_poisson", "exact_beta_poisson"))
  expect_equal(explore$models, c("exponential", "beta_poisson"))
  expect_equal(explore$method, "exhaustive")
  choices <- pool_group_choices(explore)
  iowa <- choices[grepl("^Exponential", names(choices)) & grepl("#108", names(choices))]
  keys <- pool_group_keys(explore, iowa, pool$selection)
  expect_equal(sub("\\|.*", "", keys), c("108", "139"))

  entry <- pool_explore_log_entry(explore, pool$selection, "sample")
  expect_equal(entry$kind, c("exploration", "exploration"))
  expect_equal(unique(entry$n_pairwise_tests), 10)
  expect_equal(unique(entry$partitions_considered), 52)
  expect_match(entry$groups[[1L]], "\\{#108 \\(Iowa strain\\), #139 \\(Iowa isolate\\)\\}")
})

test_that("exploration refuses sets it cannot search", {
  pool <- crypto_pool()
  datasets <- pool_datasets(pool$rows, pool$selection)
  expect_s3_class(pool_explore(datasets, "exact_beta_poisson"), "error")
  expect_s3_class(pool_explore(datasets[1L], "exponential"), "error")
})

test_that("test and exploration log rows bind together", {
  pool <- crypto_pool()
  datasets <- pool_datasets(pool$rows, pool$selection)
  explore <- pool_explore(datasets, "exponential")
  run <- pool_run(datasets[1:2], "exponential")
  rows <- rbind(
    pool_explore_log_entry(explore, pool$selection, "sample"),
    pool_log_entry(run, pool$selection[1:2, ], pool_check(pool$selection[1:2, ]), "Iowa isolates.", "sample")
  )
  expect_equal(rows$kind, c("exploration", "test"))
})

test_that("Bell numbers count the ways to split a set", {
  expect_equal(vapply(1:6, pool_bell, numeric(1L)), c(1, 2, 5, 15, 52, 203))
})

test_that("a set within an explored set is data-informed", {
  explored <- list(c("a", "b", "c"))
  expect_true(pool_data_informed(c("a", "b"), explored))
  expect_false(pool_data_informed(c("a", "d"), explored))
  expect_false(pool_data_informed(c("a", "b"), list()))
})

test_that("pooled data for Fit sum shared doses under one set of metadata", {
  pool <- crypto_pool()
  chosen <- pool$selection[1:2, ]
  data <- pool_fit_data(pool_datasets(pool$rows, chosen), chosen)
  standardized <- singlehit::as_dose_response(data)
  expect_equal(nrow(standardized), length(unique(data$dose)))
  expect_equal(sum(standardized$total), sum(data$positive + data$negative))
  expect_match(unique(standardized$study_id), "^Pooled: .*#108 \\+ .*#139$")
  expect_equal(unique(standardized$dose_unit), "oocysts")
})
