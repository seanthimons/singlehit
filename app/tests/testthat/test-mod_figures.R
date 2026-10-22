test_that("the figure follows the chosen view", {
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(type = "overlay")
    expect_s3_class(figure(), "ggplot")
    session$setInputs(type = "single", model = "exponential")
    expect_s3_class(figure(), "ggplot")
    expect_match(figure()$labels$title, "Exponential")
  })
})

test_that("a stale model selection is ignored instead of raising an error", {
  analysis <- shiny::reactiveVal(example_analysis(c("exponential", "beta_poisson")))
  shiny::testServer(mod_figures_server, args = list(analysis = analysis), {
    session$setInputs(type = "single", model = "beta_poisson")
    expect_s3_class(figure(), "ggplot")

    # Re-run without beta-Poisson; the old selection is still in `input`.
    analysis(example_analysis(c("exponential", "exact_beta_poisson")))
    session$flushReact()
    expect_error(figure(), class = "shiny.silent.error")
  })
})

test_that("with no analysis the tab shows an empty state and no figure", {
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(NULL)), {
    session$setInputs(type = "overlay")
    expect_match(as.character(output$empty$html), "Run the analysis")
    expect_error(figure(), class = "shiny.silent.error")
  })
})

test_that("the download writes a non-empty PNG and tolerates bad dimensions", {
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(type = "overlay", width = NA, height = 1000, dpi = NA)
    path <- output$download
    expect_true(file.exists(path))
    expect_gt(file.size(path), 0)
    signature <- readBin(path, "raw", 8)
    expect_equal(signature, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  })
})

test_that("the Plots tab says where confidence bands come from", {
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    session$setInputs(type = "overlay")
    expect_match(as.character(output$band_hint$html), "never shows confidence bands")
    session$setInputs(type = "panels")
    expect_match(as.character(output$band_hint$html), "Run the bootstrap")
  })
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(type = "panels")
    expect_null(output$band_hint$html)
  })
})

test_that("the single-model hint covers a model that has no bands", {
  analysis <- example_analysis_with_bands()
  analysis$bootstraps$exponential <- NULL
  shiny::testServer(mod_figures_server, args = list(analysis = shiny::reactiveVal(analysis)), {
    session$setInputs(type = "single", model = "exponential")
    expect_match(as.character(output$band_hint$html), "No confidence bands for this model")
    session$setInputs(type = "single", model = "beta_poisson")
    expect_null(output$band_hint$html)
  })
})

test_that("bands arriving while on the overlay switch the view and say so", {
  updates <- list()
  notices <- character()
  testthat::local_mocked_bindings(
    updateRadioButtons = function(session, inputId, ..., selected = NULL) {
      updates[[length(updates) + 1L]] <<- list(inputId = inputId, selected = selected)
    },
    showNotification = function(ui, ...) {
      notices <<- c(notices, as.character(ui))
    },
    .package = "singlehitapp"
  )
  analysis <- shiny::reactiveVal(example_analysis())
  shiny::testServer(mod_figures_server, args = list(analysis = analysis), {
    session$setInputs(type = "overlay")
    session$flushReact()
    expect_length(updates, 0L)

    analysis(example_analysis_with_bands())
    session$flushReact()
    expect_length(updates, 1L)
    expect_equal(updates[[1L]], list(inputId = "type", selected = "panels"))
    expect_match(notices, "switched to one panel per model")

    # The same bands again, or a rerun that still has bands, changes nothing.
    analysis(example_analysis_with_bands())
    session$flushReact()
    expect_length(updates, 1L)
  })
})

test_that("bands arriving while on the panel or single view leave the choice alone", {
  for (view in c("panels", "single")) {
    updates <- 0L
    testthat::local_mocked_bindings(
      updateRadioButtons = function(...) updates <<- updates + 1L,
      showNotification = function(...) NULL,
      .package = "singlehitapp"
    )
    analysis <- shiny::reactiveVal(example_analysis())
    shiny::testServer(mod_figures_server, args = list(analysis = analysis), {
      session$setInputs(type = view, model = "exponential")
      analysis(example_analysis_with_bands())
      session$flushReact()
    })
    expect_equal(updates, 0L)
  }
})
