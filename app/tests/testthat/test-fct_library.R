test_that("free-text metadata falls into shared groups", {
  expect_equal(
    library_host_group(c("human", "Human, immune", "mice", "C57Bl/6J Mice", "guinea pig", NA)),
    c("Human", "Human", "Mouse", "Mouse", "Other animal", "Not reported")
  )
  expect_equal(
    library_route_group(c("oral (in milk)", "intragastric", "intranasal", "aerosol", "intraperitoneal", "contact lens")),
    c("Oral", "Oral", "Respiratory", "Respiratory", "Injection", "Other")
  )
  expect_equal(
    library_outcome_group(c("death", "stillbirths", "infection", "positive stool culture", "diarrhea")),
    c("Death or mortality", "Death or mortality", "Infection or detection", "Infection or detection", "Illness or clinical signs")
  )
  expect_equal(library_dose_unit(c(" CFU ", "NR", NA, "")), c("CFU", "Not reported", "Not reported", "Not reported"))
  expect_equal(library_category(c("bacteria", "Bacteria", "virus", NA)), c("Bacteria", "Bacteria", "Virus", "Not reported"))
  expect_equal(library_model(c("exponential", "beta-Poisson", "NR", NA)), c("exponential", "beta_poisson", NA, NA))
})

test_that("workbook parameters are pulled out of free text", {
  expect_equal(library_parameter("k = 4.19E-03", "k"), 4.19e-3)
  expect_equal(library_parameter("a = 0.201 k = 4.87E-9 N50 = 1.42E+8", "N50"), 1.42e8)
  expect_equal(library_parameter("a = 0.04 N50 = 8.26E+08", "(?:a|\u03b1)"), 0.04)
  expect_true(is.na(library_parameter("NR", "k")))
  expect_true(is.na(library_parameter(NA, "k")))
})

test_that("the sample workbook reads into one row per dose group", {
  rows <- read_qmra_workbook(qmra_sample_path())
  expect_true(all(c("experiment_key", "dose", "positive", "negative", "dose_unit", "outcome_group") %in% names(rows)))
  expect_equal(length(unique(rows$experiment_key)), 14L)
  # The parameter-only experiment keeps a single row without counts.
  parameter_only <- rows[rows$experiment_id == "Ojielo2003", ]
  expect_equal(nrow(parameter_only), 1L)
  expect_true(is.na(parameter_only$dose))
  expect_true(rows$wiki_pooled[rows$experiment_id == "21,23"][[1L]])
  expect_false(any(rows$wiki_pooled[rows$experiment_id == "21"]))
})

test_that("a workbook without the raw data sheet is rejected", {
  skip_if_not_installed("writexl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(other = data.frame(x = 1)), path)
  expect_error(read_qmra_workbook(path), "raw data")
})

test_that("experiments report whether singlehit can fit them", {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  expect_equal(nrow(experiments), 14L)
  expect_false(experiments$fittable[experiments$experiment_id == "Ojielo2003"])
  expect_match(experiments$problem[experiments$experiment_id == "Ojielo2003"], "only fitted parameters")
  expect_false(experiments$fittable[experiments$pathogen == "Enterovirus"])
  expect_match(experiments$problem[experiments$pathogen == "Enterovirus"], "three distinct dose groups")
  expect_true(experiments$fittable[experiments$pathogen == "Rotavirus"])
  expect_equal(experiments$doses[experiments$experiment_id == "Ojielo2003"], 0L)
})

test_that("experiment data carry singlehit's metadata columns", {
  rows <- read_qmra_workbook(qmra_sample_path())
  key <- rows$experiment_key[rows$pathogen == "Rotavirus"][[1L]]
  data <- library_experiment_data(rows, key)
  standardized <- singlehit::as_dose_response(data)
  expect_equal(unique(standardized$dose_unit), "FFU")
  expect_equal(unique(standardized$study_id), "71")
  expect_error(library_experiment_data(rows, "nope"), "not in the workbook")
})

test_that("curves are refit from counts or drawn from workbook parameters", {
  rows <- read_qmra_workbook(qmra_sample_path())
  curves <- library_curves(rows, points = 20L)
  sources <- tapply(curves$source, curves$experiment_key, unique)
  expect_equal(unname(sources[grepl("^Ojielo2003", names(sources))]), "Workbook parameters")
  expect_equal(unname(sources[grepl("^71\\|", names(sources))]), "Refit from counts")
  expect_true(all(curves$estimate >= 0 & curves$estimate <= 1))
  expect_equal(names(attr(curves, "skipped")), "1|Variola major|death|NR")
})

test_that("the overview plot builds for every facet and colour", {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  curves <- library_curves(rows, points = 20L)
  for (facet in c("none", library_fields)) {
    plot <- library_plot(curves, experiments, facet = facet, colour = "category")
    expect_s3_class(plot, "ggplot")
    expect_no_error(ggplot2::ggplot_build(plot))
  }
  for (colour in library_fields) {
    expect_no_error(ggplot2::ggplot_build(library_plot(curves, experiments, colour = colour, free_x = TRUE)))
  }
})

test_that("the dose axis says when units differ", {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  curves <- library_curves(rows, points = 20L)
  mixed <- library_plot(curves, experiments)
  expect_match(mixed$labels$x, "study-specific units")
  expect_match(mixed$labels$caption, "facet by dose unit")
  cfu <- library_plot(curves, experiments[experiments$dose_unit == "CFU", ])
  expect_equal(cfu$labels$x, "Dose (CFU, log10 scale)")
  by_unit <- library_plot(curves, experiments, facet = "dose_unit")
  expect_match(by_unit$labels$x, "unit in panel title")
})

test_that("an unknown pathogen group still gets a colour", {
  colours <- library_palette(c("Virus", "Fungus"))
  expect_equal(names(colours), c("Fungus", "Virus"))
  expect_false(anyNA(colours))
})

test_that("plots with nothing to draw or bad fields fail clearly", {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  curves <- library_curves(rows, points = 20L)
  expect_error(library_plot(curves, experiments[0, ]), "No curves")
  expect_error(library_plot(curves, experiments, facet = "pathogen"), "facet")
  expect_error(library_plot(curves, experiments, colour = "nope"), "colour")
})
