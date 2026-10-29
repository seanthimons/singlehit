pooling_args <- function() {
  rows <- shiny::reactiveVal(read_qmra_workbook(qmra_sample_path()))
  list(
    rows = rows,
    # Built from the rows, as on the Library tab, so a new workbook brings new
    # experiments.
    experiments = shiny::reactive(library_experiments(rows())),
    pool_set = shiny::reactiveVal(NULL),
    workbook_name = shiny::reactive("the sample workbook"),
    workbook_loaded = shiny::reactiveVal(1L)
  )
}

keys_for <- function(experiments, ids) experiments$experiment_key[experiments$experiment_id %in% ids]

test_that("the test runs only with a rationale and no blocking check", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = keys_for(experiments(), c("21", "23")), sent = 1L))
    session$flushReact()
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, rationale = "")
    expect_false(ready())

    # Pressing Run without a rationale does nothing.
    session$setInputs(run = 1)
    expect_null(result())
    expect_null(log())

    session$setInputs(rationale = "Same pathogen, route, and outcome.")
    expect_true(ready())
    session$setInputs(run = 2)
    expect_false(inherits(result()$run, "error"))
    expect_equal(nrow(log()), 2L)
    expect_equal(unique(log()$run), 1L)
    expect_equal(unique(log()$workbook), "the sample workbook")

    session$setInputs(run = 3)
    expect_equal(unique(log()$run), c(1L, 2L))
  })
})

test_that("a blocking check keeps the test from running", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = keys_for(experiments(), c("21", "71")), sent = 1L))
    session$setInputs(models = "exponential", alpha = 0.05, rationale = "Testing.")
    expect_true(pool_blocked(checks()))
    expect_false(ready())
    session$setInputs(run = 1)
    expect_null(result())
  })
})

test_that("a new workbook clears the selection and results but keeps the log", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = keys_for(experiments(), c("21", "23")), sent = 1L))
    session$flushReact()
    session$setInputs(models = "beta_poisson", alpha = 0.05, rationale = "Same pathogen.")
    session$setInputs(run = 1)
    expect_equal(nrow(log()), 1L)

    # Reloading the same file leaves the rows identical; the load counter still
    # starts a new analysis.
    args$rows(read_qmra_workbook(qmra_sample_path()))
    args$workbook_loaded(2L)
    session$flushReact()
    expect_equal(keys(), character())
    expect_null(result())
    expect_equal(nrow(log()), 1L)
  })
})

test_that("a new pooling set needs its own rationale", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = keys_for(experiments(), c("21", "23")), sent = 1L))
    session$flushReact()
    session$setInputs(models = "beta_poisson", alpha = 0.05, rationale = "Same pathogen.")
    expect_true(ready())

    args$pool_set(list(keys = keys_for(experiments(), c("23", "21")), sent = 2L))
    session$flushReact()
    # The old text is still in the input here (testServer does not apply the
    # update that clears it), but it no longer counts.
    expect_equal(rationale(), "")
    expect_false(ready())
    session$setInputs(run = 1)
    expect_null(result())

    session$setInputs(rationale = "Same pathogen, checked again.")
    expect_true(ready())
  })
})

test_that("the log downloads as CSV", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    empty <- read.csv(output$download_log)
    expect_equal(names(empty), "note")

    args$pool_set(list(keys = keys_for(experiments(), c("21", "23")), sent = 1L))
    session$flushReact()
    session$setInputs(models = "beta_poisson", alpha = 0.05, rationale = "Same pathogen.")
    session$setInputs(run = 1)
    logged <- read.csv(output$download_log)
    expect_equal(nrow(logged), 1L)
    expect_true(all(c("run", "workbook", "experiments", "rationale", "p_value") %in% names(logged)))
  })
})

test_that("loading a different workbook does not trip over the old selection", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = keys_for(experiments(), c("21", "23")), sent = 1L))
    session$flushReact()
    session$setInputs(models = "beta_poisson", alpha = 0.05, rationale = "Same pathogen.")
    expect_true(ready())

    # A workbook without 21 and 23, set before the load counter as the Data
    # tab does.
    rows <- read_qmra_workbook(qmra_sample_path())
    args$rows(rows[!rows$experiment_id %in% c("21", "23"), ])
    args$workbook_loaded(2L)
    expect_no_error(session$flushReact())
    expect_equal(keys(), character())
  })
})

crypto_keys <- function(experiments) experiments$experiment_key[grepl("Cryptosporidium", experiments$pathogen)]

test_that("a group taken from an exploration is marked data-informed and needs a rationale", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = crypto_keys(experiments()), sent = 1L))
    session$flushReact()
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05)
    session$setInputs(explore = 1)
    expect_false(inherits(exploration()$explore, "error"))
    expect_equal(unique(log()$kind), "exploration")
    # The first exploration of a Library set is logged as a Library set.
    expect_equal(unique(log()$set_origin), "Library")

    choices <- pool_group_choices(exploration()$explore)
    session$setInputs(group = choices[[1L]], use_group = 1)
    expect_equal(sub("\\|.*", "", keys()), c("108", "139"))
    expect_equal(set_origin(), "exploratory grouping (Exponential)")
    expect_false(ready())

    session$setInputs(rationale = "Both Iowa isolate.")
    session$setInputs(run = 1)
    tests <- log()[log()$kind == "test", ]
    expect_equal(unique(tests$set_origin), "exploratory grouping (Exponential)")

    session$setInputs(send = 1)
    expect_match(unique(session$returned$sent()$data$study_id), "\\[data-informed set\\]$")
  })
})

test_that("re-picking an explored set on the Library tab stays data-informed", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    all <- crypto_keys(experiments())
    args$pool_set(list(keys = all, sent = 1L))
    session$flushReact()
    session$setInputs(models = "exponential", alpha = 0.05)
    session$setInputs(explore = 1)

    args$pool_set(list(keys = all[1:2], sent = 2L))
    session$flushReact()
    expect_equal(set_origin(), "Library, within a set explored this session")
  })
})

test_that("pooled data are sent on after a test, with their origin in the label", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = crypto_keys(experiments())[1:2], sent = 1L))
    session$flushReact()
    session$setInputs(models = "exponential", alpha = 0.05, rationale = "Both Iowa isolate.")
    session$setInputs(send = 1)
    expect_null(session$returned$sent())

    session$setInputs(run = 1)
    session$setInputs(send = 2)
    sent <- session$returned$sent()
    expect_equal(sent$source, "Pooling tab")
    expect_match(sent$label, "^Pooled: ")
    expect_no_error(singlehit::as_dose_response(sent$data))
    expect_true("sent to Fit" %in% log()$kind)
  })
})

test_that("reloading the workbook does not clear what was explored", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    all <- crypto_keys(experiments())
    args$pool_set(list(keys = all, sent = 1L))
    session$flushReact()
    session$setInputs(models = "exponential", alpha = 0.05)
    session$setInputs(explore = 1)

    args$workbook_loaded(2L)
    session$flushReact()
    args$pool_set(list(keys = all[1:2], sent = 2L))
    session$flushReact()
    expect_equal(set_origin(), "Library, within a set explored this session")
  })
})

test_that("the status counts an exploration's pairwise tests and says they are unadjusted", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = crypto_keys(experiments()), sent = 1L))
    session$flushReact()
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05)
    expect_false(grepl("adjusted", output$log_status$html))
    session$setInputs(explore = 1)
    # Five experiments: 10 pairs under each of two models.
    expect_match(output$log_status$html, "20 pairwise tests")
    expect_match(output$log_status$html, "None of the p-values are adjusted")
  })
})

test_that("the exploration shows how to read the heatmap", {
  args <- pooling_args()
  shiny::testServer(mod_pooling_server, args = args, {
    args$pool_set(list(keys = crypto_keys(experiments()), sent = 1L))
    session$flushReact()
    session$setInputs(models = "exponential", alpha = 0.05)
    session$setInputs(explore = 1)
    html <- output$explore_results$html
    expect_match(html, "How to read this heatmap")
    expect_match(html, "articles/pooling.html")
  })
})
