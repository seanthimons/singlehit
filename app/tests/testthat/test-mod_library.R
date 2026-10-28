test_that("the sample workbook loads and the filters narrow the experiments", {
  shiny::testServer(mod_library_server, {
    session$setInputs(sample = 1)
    expect_equal(nrow(experiments()), 14L)
    expect_equal(nrow(filtered()), 14L)

    session$setInputs(filter_category = "Protozoa")
    expect_equal(nrow(filtered()), 3L)
    session$setInputs(filter_category = character(), filter_dose_unit = c("CFU", "PFU"))
    expect_setequal(unique(filtered()$dose_unit), c("CFU", "PFU"))
    session$setInputs(filter_dose_unit = character(), hide_wiki_pooled = TRUE)
    expect_false(any(filtered()$wiki_pooled))
  })
})

test_that("sending an experiment returns its data with a changing counter", {
  shiny::testServer(mod_library_server, {
    session$setInputs(sample = 1)
    key <- experiments()$experiment_key[experiments()$pathogen == "Rotavirus"]
    session$setInputs(experiment = key, send = 1)
    first <- session$returned()
    expect_equal(first$sent, 1L)
    expect_match(first$label, "Rotavirus")
    expect_equal(unique(first$data$dose_unit), "FFU")

    session$setInputs(send = 2)
    expect_equal(session$returned()$sent, 2L)
  })
})

test_that("an experiment that cannot be fitted is not sent", {
  shiny::testServer(mod_library_server, {
    session$setInputs(sample = 1)
    key <- experiments()$experiment_key[experiments()$experiment_id == "Ojielo2003"]
    session$setInputs(experiment = key, send = 1)
    expect_null(session$returned())
  })
})

test_that("a file that is not a QMRA workbook shows an error", {
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(other = data.frame(x = 1)), path)
  shiny::testServer(mod_library_server, {
    session$setInputs(file = list(datapath = path))
    expect_s3_class(rows(), "error")
  })
})
