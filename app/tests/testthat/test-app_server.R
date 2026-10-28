test_that("an experiment sent from the Library becomes the current dataset", {
  shiny::testServer(app_server, {
    session$setInputs(`upload-sample_workbook` = 1)
    key <- "71|Rotavirus|infection|beta-Poisson"
    session$setInputs(`library-experiment` = key, `library-send` = 1)
    expect_equal(nrow(data()), 8L)
    expect_equal(unique(data()$dose_unit), "FFU")
    expect_false(is.null(upload$workbook()))
  })
})

test_that("a pooling set chosen on the Library reaches the Pooling tab", {
  shiny::testServer(app_server, {
    session$setInputs(`upload-sample_workbook` = 1)
    keys <- c("21|Burkholderia mallei|death|beta-Poisson", "23|Burkholderia mallei|death|beta-Poisson")
    session$setInputs(`library-pool_set` = keys, `library-pool` = 1)
    expect_equal(library_tab$pool_set()$keys, keys)
  })
})

test_that("pooled data sent from the Pooling tab become the current dataset", {
  shiny::testServer(app_server, {
    session$setInputs(`upload-sample_workbook` = 1)
    keys <- c(
      "108|Cryptosporidium hominis/ Cryptosporidium parvum|infection|exponential",
      "139|Cryptosporidium hominis/ Cryptosporidium parvum|infection|exponential"
    )
    session$setInputs(`library-pool_set` = keys, `library-pool` = 1)
    session$setInputs(`pooling-models` = "exponential", `pooling-alpha` = 0.05)
    session$setInputs(`pooling-rationale` = "Both Iowa isolate.")
    session$setInputs(`pooling-run` = 1)
    session$setInputs(`pooling-send` = 1)
    expect_match(unique(data()$study_id), "^Pooled: ")
    expect_equal(unique(data()$dose_unit), "oocysts")
    expect_setequal(unique(upload$components()$experiment), c("#108 (Iowa strain)", "#139 (Iowa isolate)"))
  })
})
