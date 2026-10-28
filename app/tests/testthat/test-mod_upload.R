test_that("the example data loads through singlehit's public reader", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(example = 1)
    data <- session$returned$data()
    expect_s3_class(data, "tbl_df")
    expect_true(all(c("dose", "positive", "negative", "total", "response") %in% names(data)))
  })
})

test_that("invalid uploads return NULL instead of failing the session", {
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("dose,positive", "1,2"), bad)
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = bad))
    expect_null(session$returned$data())
  })
})

test_that("column mapping reads files with unrecognized column names", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("conc,infected,healthy", "1,1,9", "10,5,5", "100,9,1"), path)
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = path))
    expect_equal(columns(), c("conc", "infected", "healthy"))
    expect_null(session$returned$data())

    session$setInputs(dose = "conc", positive = "infected", negative = "healthy")
    data <- session$returned$data()
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
    expect_false(is.null(session$returned$data()))

    # The stale selections are still in `input`, as they would be for a moment
    # in a real session; the example must still parse with auto-detection.
    session$setInputs(example = 1)
    expect_true(all(c("dose", "positive", "negative") %in% names(session$returned$data())))
    expect_equal(nrow(session$returned$data()), 8L)
  })
})

test_that("the example button loads the Ward rotavirus data", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(example = 1)
    data <- session$returned$data()
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
    result <- session$returned$data()
    expect_equal(result$dose, c(1, 10, 100))
    expect_equal(unique(result$dose_unit), "CFU")
    expect_match(loaded()$label, "Test experiment from the Library tab")

    # Loading the example afterwards replaces the library data.
    session$setInputs(example = 1)
    expect_equal(loaded()$label, "Ward rotavirus example data")
    expect_equal(nrow(session$returned$data()), 8L)
  })
})

test_that("the sample workbook loads as a workbook and leaves the dataset alone", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(example = 1)
    session$setInputs(sample_workbook = 1)
    expect_equal(length(unique(session$returned$workbook()$experiment_key)), 14L)
    expect_equal(session$returned$workbook_loaded(), 1L)
    expect_equal(nrow(session$returned$data()), 8L)

    # Loading the same workbook again still counts as a new load.
    session$setInputs(sample_workbook = 2)
    expect_equal(session$returned$workbook_loaded(), 2L)
  })
})

test_that("an uploaded .xlsx is read as a workbook, whatever the case of its extension", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(file = list(datapath = qmra_sample_path(), name = "QMRA.XLSX"))
    expect_equal(length(unique(session$returned$workbook()$experiment_key)), 14L)
    expect_null(loaded())
  })
})

test_that("a delimited file after a workbook keeps the workbook", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("dose,positive,negative", "1,1,9", "10,5,5", "100,9,1"), path)
  shiny::testServer(mod_upload_server, {
    session$setInputs(sample_workbook = 1)
    session$setInputs(file = list(datapath = path, name = "trial.csv"))
    expect_equal(nrow(session$returned$data()), 3L)
    expect_false(is.null(session$returned$workbook()))
    expect_equal(loaded()$label, "trial.csv")
  })
})

test_that("a bad workbook reports an error and keeps the previous workbook", {
  skip_if_not_installed("writexl")
  bad <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(other = data.frame(x = 1)), bad)
  shiny::testServer(mod_upload_server, {
    session$setInputs(sample_workbook = 1)
    session$setInputs(file = list(datapath = bad, name = "bad.xlsx"))
    expect_match(workbook_error(), "raw data")
    expect_equal(length(unique(session$returned$workbook()$experiment_key)), 14L)
    expect_equal(session$returned$workbook_loaded(), 1L)

    # The error is stale once a dataset loads.
    session$setInputs(example = 1)
    expect_null(workbook_error())
  })
})
