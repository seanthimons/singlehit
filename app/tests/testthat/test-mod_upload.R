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
