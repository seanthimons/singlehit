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
    expect_equal(nrow(session$returned()), 8L)
  })
})

test_that("the example button loads the Ward rotavirus data", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(example = 1)
    data <- session$returned()
    expect_equal(nrow(data), 8L)
    expect_equal(range(data$dose), c(0.009, 90000))
    expect_equal(sum(data$total), 59)
  })
})

test_that("data sent from the Library tab load without a file", {
  sent <- shiny::reactiveVal(NULL)
  data <- data.frame(dose = c(1, 10, 100), positive = c(1, 5, 9), negative = c(9, 5, 1), study_id = "x", dose_unit = "CFU")
  shiny::testServer(mod_upload_server, args = list(incoming = sent), {
    sent(list(data = data, label = "Test experiment", sent = 1L))
    session$flushReact()
    result <- session$returned()
    expect_equal(result$dose, c(1, 10, 100))
    expect_equal(unique(result$dose_unit), "CFU")
    expect_true(is.list(loaded()))

    # Loading the example afterwards replaces the library data.
    session$setInputs(example = 1)
    expect_false(is.list(loaded()))
    expect_equal(nrow(session$returned()), 8L)
  })
})
