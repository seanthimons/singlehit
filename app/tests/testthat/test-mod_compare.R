test_that("the primary recommendation is shown and the consensus is labelled experimental", {
  analysis <- shiny::reactiveVal(example_analysis())
  shiny::testServer(mod_compare_server, args = list(analysis = analysis), {
    html <- as.character(output$decision$html)
    expect_match(html, "Recommended model: Exponential")
    expect_match(html, "Experimental")
    expect_true(regexpr("Model assessment", html) < regexpr("Consensus selection", html))
  })
})

test_that("with no analysis the tab shows an empty state", {
  shiny::testServer(mod_compare_server, args = list(analysis = shiny::reactiveVal(NULL)), {
    expect_match(as.character(output$empty$html), "Run the analysis")
  })
})

test_that("the consensus table marks the selected model", {
  analysis <- shiny::reactiveVal(example_analysis())
  shiny::testServer(mod_compare_server, args = list(analysis = analysis), {
    table <- consensus()
    expect_equal(table$model[table$consensus_selected], "beta_poisson")
  })
})
