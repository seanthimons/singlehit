sample_rows <- function() {
  rows <- read_qmra_workbook(qmra_sample_path())
  shiny::reactive(rows)
}

test_that("the sample workbook loads and the filters narrow the experiments", {
  shiny::testServer(mod_library_server, args = list(rows = sample_rows()), {
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
  shiny::testServer(mod_library_server, args = list(rows = sample_rows()), {
    key <- experiments()$experiment_key[experiments()$pathogen == "Rotavirus"]
    session$setInputs(experiment = key, send = 1)
    first <- session$returned$sent()
    expect_equal(first$sent, 1L)
    expect_match(first$label, "Rotavirus")
    expect_equal(unique(first$data$dose_unit), "FFU")

    session$setInputs(send = 2)
    expect_equal(session$returned$sent()$sent, 2L)
  })
})

test_that("an experiment that cannot be fitted is not sent", {
  shiny::testServer(mod_library_server, args = list(rows = sample_rows()), {
    key <- experiments()$experiment_key[experiments()$experiment_id == "Ojielo2003"]
    session$setInputs(experiment = key, send = 1)
    expect_null(session$returned$sent())
  })
})

test_that("the Library is empty until a workbook is loaded", {
  rows <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_library_server, args = list(rows = rows), {
    expect_null(session$returned$sent())
    rows(read_qmra_workbook(qmra_sample_path()))
    session$flushReact()
    expect_equal(nrow(experiments()), 14L)
  })
})

test_that("the pooling set offers only eligible experiments and is handed on", {
  shiny::testServer(mod_library_server, args = list(rows = sample_rows()), {
    session$flushReact()
    expect_null(session$returned$pool_set())
    keys <- experiments()$experiment_key[experiments()$experiment_id %in% c("21", "23")]
    session$setInputs(pool_set = keys, pool = 1)
    expect_equal(session$returned$pool_set()$keys, keys)
    expect_equal(session$returned$pool_set()$sent, 1L)
    expect_equal(nrow(session$returned$experiments()), 14L)
  })
})

test_that("pressing Test poolability with nothing chosen hands nothing on", {
  shiny::testServer(mod_library_server, args = list(rows = sample_rows()), {
    session$setInputs(pool = 1)
    expect_null(session$returned$pool_set())
  })
})
