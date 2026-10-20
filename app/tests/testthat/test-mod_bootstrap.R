# These tests run real jobs on a tiny shared worker pool. 100 replicates is the
# smallest allowed run: 5 batches per model.
wait_for <- function(session, condition, tries = 150) {
  for (i in seq_len(tries)) {
    session$elapse(600)
    if (condition()) {
      return(TRUE)
    }
    Sys.sleep(0.2)
  }
  FALSE
}

local_workers <- function(workers = 2, env = parent.frame()) {
  skip_if_not_installed("mirai")
  withr::local_options(singlehitapp.workers = workers, .local_envir = env)
  withr::defer(stop_bootstrap_workers(), envir = env)
}

test_that("a run completes and the returned analysis carries the bootstraps", {
  local_workers()
  analysis <- shiny::reactiveVal(example_analysis())
  shiny::testServer(mod_bootstrap_server, args = list(analysis = analysis), {
    expect_length(session$returned()$bootstraps, 0L)

    session$setInputs(times = 100, resample = "observed", seed = 1, run = 1)
    expect_equal(state$status, "running")
    expect_true(wait_for(session, function() state$status != "running"))
    expect_equal(state$status, "done")

    returned <- session$returned()
    expect_setequal(names(returned$bootstraps), c("exponential", "beta_poisson"))
    expect_equal(nrow(returned$bootstraps$exponential), 100L)
    expect_equal(state$progress$exponential$done, state$progress$exponential$total)
    expect_s3_class(singlehit::bootstrap_confint(returned$bootstraps$beta_poisson), "data.frame")
  })
})

test_that("the same seed gives the same replicates", {
  local_workers()
  run_once <- function() {
    deviance <- NULL
    shiny::testServer(
      mod_bootstrap_server,
      args = list(analysis = shiny::reactiveVal(example_analysis())),
      {
        session$setInputs(times = 100, resample = "observed", seed = 42, run = 1)
        wait_for(session, function() state$status != "running")
        deviance <<- session$returned()$bootstraps$beta_poisson$deviance
      }
    )
    deviance
  }
  first <- run_once()
  second <- run_once()
  expect_length(first, 100L)
  expect_equal(first, second)
})

test_that("invalid settings show an error and start nothing", {
  local_workers()
  shiny::testServer(mod_bootstrap_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(times = 10, resample = "observed", seed = NA, run = 1)
    expect_equal(state$status, "error")
    expect_match(state$message, "between 100 and 10,000")
    expect_length(jobs(), 0L)
    expect_false(mirai::daemons_set(.compute = bootstrap_compute_profile))
  })
})

test_that("running with no analysis is refused", {
  local_workers()
  shiny::testServer(mod_bootstrap_server, args = list(analysis = shiny::reactiveVal(NULL)), {
    session$setInputs(times = 100, resample = "observed", seed = NA, run = 1)
    expect_equal(state$status, "idle")
    expect_length(jobs(), 0L)
    expect_null(session$returned())
  })
})

test_that("a second Run while running is ignored", {
  # One worker and 1000 replicates per model keeps the run going long enough.
  local_workers(1)
  shiny::testServer(mod_bootstrap_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(times = 1000, resample = "observed", seed = 1, run = 1)
    expect_equal(state$status, "running")
    first <- jobs()
    session$setInputs(run = 2)
    expect_identical(jobs(), first)
    session$setInputs(cancel = 1)
    expect_equal(state$status, "cancelled")
  })
})

test_that("cancelling stops the run and adds no bands", {
  local_workers(1)
  shiny::testServer(mod_bootstrap_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(times = 1000, resample = "observed", seed = 1, run = 1)
    expect_equal(state$status, "running")

    session$setInputs(cancel = 1)
    expect_equal(state$status, "cancelled")
    expect_length(jobs(), 0L)
    # Stopped jobs report as finished; the poll must not collect them.
    session$elapse(1200)
    expect_equal(state$status, "cancelled")
    expect_length(session$returned()$bootstraps, 0L)
  })
})

test_that("a new analysis cancels a run and discards finished results", {
  local_workers()
  analysis <- shiny::reactiveVal(example_analysis())
  shiny::testServer(mod_bootstrap_server, args = list(analysis = analysis), {
    session$setInputs(times = 100, resample = "observed", seed = 1, run = 1)
    expect_true(wait_for(session, function() state$status != "running"))
    expect_equal(state$status, "done")
    expect_length(session$returned()$bootstraps, 2L)

    analysis(example_analysis(c("exponential", "exact_beta_poisson")))
    session$flushReact()
    expect_equal(state$status, "idle")
    expect_length(session$returned()$bootstraps, 0L)
  })
})

test_that("the Run buttons are task buttons that turn busy as soon as they are clicked", {
  bootstrap_html <- as.character(mod_bootstrap_ui("bootstrap"))
  expect_match(bootstrap_html, "bslib-task-button")
  expect_match(bootstrap_html, "Preparing replicates...", fixed = TRUE)

  fit_html <- as.character(mod_fit_ui("fit"))
  expect_match(fit_html, "bslib-task-button")
  expect_match(fit_html, "Fitting models...", fixed = TRUE)
})

test_that("the app turns on Shiny's busy indicators", {
  expect_match(as.character(app_ui(NULL)), "shiny-busy", fixed = TRUE)
})
