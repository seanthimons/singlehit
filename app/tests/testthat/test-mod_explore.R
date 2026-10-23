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

test_that("the Explore tab explains the log axis and how to draw confidence bands", {
  html <- as.character(mod_explore_ui("explore"))
  expect_match(html, "X-axis transform", fixed = TRUE)
  expect_match(html, "map lower to ymin and upper to ymax", fixed = TRUE)
  expect_match(html, "Ribbon", fixed = TRUE)
})

# The preset drives esquisse's own inputs; these tests check the sequence of
# messages. That esquisse then draws the bands was checked in a browser.
local_preset_recorder <- function(env = parent.frame()) {
  calls <- new.env()
  calls$log <- list()
  # esquisse makes the same update calls for its own inputs; keep only ours.
  ours <- c("dataset", unlist(esquisse_ids))
  record <- function(what) function(session, inputId, ...) {
    if (inputId %in% ours) {
      calls$log[[length(calls$log) + 1L]] <- list(what = what, id = inputId, args = list(...))
    }
  }
  testthat::local_mocked_bindings(updateSelectInput = record("dataset"), updateSelectizeInput = record("axis"),
    .package = "singlehitapp", .env = env)
  testthat::local_mocked_bindings(updateDragulaInput = record("aesthetics"), updateDropInput = record("geom"),
    .package = "esquisse", .env = env)
  calls
}
sent <- function(calls) vapply(calls$log, `[[`, "", "what")
mapping_input <- function() {
  c(list(xvar = "dose", yvar = "estimate"), list(ymin = "lower", ymax = "upper", fill = "level", facet = "model"))
}
mapping_id <- "esquisse-geomaes-aes_1-dragvars"

test_that("the bands button only appears once a bootstrap is attached", {
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis())), {
    expect_equal(output$has_bands, "no")
  })
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    expect_equal(output$has_bands, "yes")
  })
  html <- as.character(mod_explore_ui("explore"))
  expect_match(html, "explore-bands_preset", fixed = TRUE)
  expect_match(html, "output.has_bands == &#39;yes&#39;", fixed = TRUE)
})

test_that("the bands preset switches the dataset, then the mapping, then the geometry and axis", {
  calls <- local_preset_recorder()
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(dataset = "observed")
    session$setInputs(bands_preset = 1)
    expect_equal(sent(calls), "dataset")
    expect_equal(calls$log[[1]]$args$selected, "curves")

    session$setInputs(dataset = "curves")
    session$flushReact()
    expect_equal(sent(calls), c("dataset", "aesthetics"))
    expect_equal(calls$log[[2]]$id, mapping_id)
    expect_equal(calls$log[[2]]$args$selected, bands_preset$aesthetics)

    # esquisse first reports a stale mapping; nothing more is sent for it.
    stale <- list(target = list(xvar = "dose"))
    do.call(session$setInputs, setNames(list(stale), mapping_id))
    session$flushReact()
    expect_equal(sent(calls), c("dataset", "aesthetics"))

    do.call(session$setInputs, setNames(list(list(target = mapping_input())), mapping_id))
    session$flushReact()
    expect_equal(sent(calls), c("dataset", "aesthetics", "geom", "axis"))
    expect_equal(calls$log[[3]]$args$selected, "ribbon")
    expect_equal(calls$log[[4]]$args$selected, "log10")
  })
})

test_that("the bands preset skips straight to the mapping when the curves are already shown", {
  calls <- local_preset_recorder()
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(dataset = "curves")
    session$setInputs(bands_preset = 1)
    session$flushReact()
    expect_equal(sent(calls), "aesthetics")
  })
})

test_that("pressing the bands preset again only resets the geometry and axis", {
  calls <- local_preset_recorder()
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(dataset = "curves")
    do.call(session$setInputs, setNames(list(list(target = mapping_input())), mapping_id))
    session$setInputs(bands_preset = 1)
    session$flushReact()
    expect_equal(sent(calls), c("geom", "axis"))
  })
})

test_that("changing the mapping by hand does not trigger the preset", {
  calls <- local_preset_recorder()
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(dataset = "curves")
    do.call(session$setInputs, setNames(list(list(target = mapping_input())), mapping_id))
    session$flushReact()
    expect_length(calls$log, 0L)
  })
})

test_that("a pending preset is dropped when the analysis is re-run", {
  calls <- local_preset_recorder()
  analysis <- shiny::reactiveVal(example_analysis_with_bands())
  shiny::testServer(mod_explore_server, args = list(analysis = analysis), {
    session$setInputs(dataset = "curves")
    session$setInputs(bands_preset = 1)
    session$flushReact()
    expect_equal(sent(calls), "aesthetics")

    # A different analysis: a reactiveVal ignores an identical value.
    rerun <- example_analysis_with_bands()
    rerun$note <- "rerun"
    analysis(rerun)
    session$flushReact()
    do.call(session$setInputs, setNames(list(list(target = mapping_input())), mapping_id))
    session$flushReact()
    expect_equal(sent(calls), "aesthetics")
  })
})

test_that("a pending preset is dropped when the user leaves the fitted curves", {
  calls <- local_preset_recorder()
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(example_analysis_with_bands())), {
    session$setInputs(dataset = "curves")
    session$setInputs(bands_preset = 1)
    session$flushReact()
    session$setInputs(dataset = "observed")
    # esquisse warns about a mapping whose columns the observed data lacks.
    suppressWarnings({
      do.call(session$setInputs, setNames(list(list(target = mapping_input())), mapping_id))
      session$flushReact()
    })
    expect_equal(sent(calls), "aesthetics")
  })
})

test_that("the bands button is hidden when the bootstrap gives no bands", {
  analysis <- example_analysis_with_bands()
  analysis$bootstraps$exponential$converged <- FALSE
  analysis$bootstraps$beta_poisson$converged <- FALSE
  shiny::testServer(mod_explore_server, args = list(analysis = shiny::reactiveVal(analysis)), {
    expect_equal(output$has_bands, "no")
  })
})
