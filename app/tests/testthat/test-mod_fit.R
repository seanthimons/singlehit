test_that("running the analysis returns a qdr_analysis without bootstrapping", {
  shiny::testServer(mod_fit_server, args = list(data = shiny::reactiveVal(example_data())), {
    expect_null(session$returned())
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, run = 1)
    analysis <- session$returned()
    expect_s3_class(analysis, "qdr_analysis")
    expect_setequal(names(analysis$fits), c("exponential", "beta_poisson"))
    expect_length(analysis$bootstraps, 0L)
  })
})

test_that("fewer than two models is refused with a message", {
  shiny::testServer(mod_fit_server, args = list(data = shiny::reactiveVal(example_data())), {
    session$setInputs(models = "exponential", alpha = 0.05, run = 1)
    expect_null(session$returned())
    expect_match(as.character(output$status$html), "at least two models")
  })
})

test_that("an invalid alpha is refused with a message", {
  shiny::testServer(mod_fit_server, args = list(data = shiny::reactiveVal(example_data())), {
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 2, run = 1)
    expect_null(session$returned())
    expect_match(as.character(output$status$html), "between 0 and 1")
  })
})

test_that("a dataset without an increasing trend still fits and reports the trend", {
  flat <- singlehit::as_dose_response(
    data.frame(dose = c(1, 10, 100), positive = c(5, 5, 5), negative = c(5, 5, 5))
  )
  shiny::testServer(mod_fit_server, args = list(data = shiny::reactiveVal(flat)), {
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, run = 1)
    analysis <- session$returned()
    expect_s3_class(analysis, "qdr_analysis")
    expect_false(analysis$trend$passes)
    expect_match(as.character(output$trend$html), "No statistically significant increasing trend")
  })
})

test_that("an analysis error is shown instead of failing the session", {
  broken <- shiny::reactiveVal(data.frame(x = 1))
  shiny::testServer(mod_fit_server, args = list(data = broken), {
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, run = 1)
    expect_null(session$returned())
    expect_match(as.character(output$status$html), "alert-danger")
  })
})

test_that("changing the data clears the previous analysis", {
  data <- shiny::reactiveVal(example_data())
  shiny::testServer(mod_fit_server, args = list(data = data), {
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, run = 1)
    expect_s3_class(session$returned(), "qdr_analysis")

    data(NULL)
    session$flushReact()
    expect_null(session$returned())
  })
})

test_that("the Ward example: exponential fits badly and beta-Poisson is recommended", {
  shiny::testServer(mod_fit_server, args = list(data = shiny::reactiveVal(ward_data())), {
    session$setInputs(models = c("exponential", "beta_poisson"), alpha = 0.05, run = 1)
    analysis <- session$returned()
    expect_s3_class(analysis, "qdr_analysis")
    expect_true(analysis$trend$passes)
    expect_equal(headline_decision(analysis$assessment)$model, "Beta-Poisson")
    exponential <- analysis$goodness_of_fit[analysis$goodness_of_fit$model == "exponential", ]
    expect_false(exponential$good_fit)
  })
})
