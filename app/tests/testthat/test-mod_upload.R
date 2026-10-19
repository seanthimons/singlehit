test_that("the example data loads through singlehit's public reader", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(example = 1)
    data <- session$returned()
    expect_s3_class(data, "tbl_df")
    expect_true(all(c("dose", "positive", "negative", "total", "response") %in% names(data)))
  })
})

test_that("invalid uploads return NULL instead of failing the session", {
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("dose,positive", "1,2"), bad)
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = bad))
    expect_null(session$returned())
  })
})

test_that("column mapping reads files with unrecognized column names", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("conc,infected,healthy", "1,1,9", "10,5,5", "100,9,1"), path)
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = path))
    expect_equal(columns(), c("conc", "infected", "healthy"))
    expect_null(session$returned())

    session$setInputs(dose = "conc", positive = "infected", negative = "healthy")
    data <- session$returned()
    expect_equal(data$dose, c(1, 10, 100))
    expect_equal(data$positive, c(1, 5, 9))
  })
})

test_that("loading a new source discards the previous column mapping", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("conc,infected,healthy", "1,1,9", "10,5,5", "100,9,1"), path)
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = path))
    session$setInputs(dose = "conc", positive = "infected", negative = "healthy")
    expect_false(is.null(session$returned()))

    # The stale selections are still in `input`, as they would be for a moment
    # in a real session; the example must still parse with auto-detection.
    session$setInputs(example = 1)
    expect_true(all(c("dose", "positive", "negative") %in% names(session$returned())))
    expect_equal(nrow(session$returned()), 3L)
  })
})
