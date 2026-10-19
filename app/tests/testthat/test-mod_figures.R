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
