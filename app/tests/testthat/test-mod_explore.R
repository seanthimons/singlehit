test_that("the explored data follows the dataset picker", {
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(dataset = "observed")
    expect_equal(nrow(dataset()), 3L)
    expect_true("response" %in% names(dataset()))

    session$setInputs(dataset = "curves")
    expect_true("estimate" %in% names(dataset()))

    session$setInputs(dataset = "model_residuals")
    expect_true(".resid" %in% names(dataset()))
  })
})

test_that("a new analysis replaces the explored data", {
  analysis <- shiny::reactiveVal(example_analysis(c("exponential", "beta_poisson")))
  shiny::testServer(mod_explore_server, args = list(analysis = analysis), {
    session$setInputs(dataset = "curves")
    expect_equal(output$has_analysis, "yes")
    expect_setequal(dataset()$model, c("Exponential", "Beta-Poisson"))

    analysis(example_analysis(c("exponential", "exact_beta_poisson")))
    session$flushReact()
    expect_setequal(dataset()$model, c("Exponential", "Exact beta-Poisson"))
  })
})

test_that("without an analysis there is nothing to explore and the empty state shows", {
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(NULL)), {
    session$setInputs(dataset = "curves")
    expect_equal(output$has_analysis, "no")
    expect_error(dataset(), class = "shiny.silent.error")
  })
})

test_that("the Explore UI hides esquisse's close, import and editing buttons", {
  html <- as.character(mod_explore_ui("explore"))
  expect_false(grepl("explore-esquisse-close", html, fixed = TRUE))
  expect_false(grepl("explore-esquisse-launch_import_data", html, fixed = TRUE))
  expect_false(grepl("explore-esquisse-update_variable", html, fixed = TRUE))
  expect_true(grepl("explore-esquisse-show_data", html, fixed = TRUE))
})

test_that("the esquisse wrapper keeps its height inside a fillable page", {
  html <- as.character(mod_explore_ui("explore"))
  expect_match(html, "height: 700px; flex: 0 0 auto;", fixed = TRUE)
})
