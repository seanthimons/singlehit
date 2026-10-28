example_data <- function() {
  singlehit::read_dose_response(system.file("extdata", "dose-response-example.csv", package = "singlehit"))
}

example_analysis <- function(models = c("exponential", "beta_poisson")) {
  singlehit::analyze_dose_response(
    example_data(),
    models = models,
    bootstrap_times = 0L,
    exact_bootstrap_times = 0L,
    check_trend = FALSE
  )
}

ward_data <- function() {
  singlehit::read_dose_response(system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"))
}

# Small, fast bootstrap (sequential, in-process) attached to the tiny example.
example_analysis_with_bands <- function(models = c("exponential", "beta_poisson")) {
  analysis <- example_analysis(models)
  boots <- singlehit::bootstrap_dose_response_models(analysis$fits, times = 20, seed = 1, backend = "sequential")
  singlehit::add_bootstrap(analysis, boots)
}

qmra_sample_path <- function() {
  system.file("extdata", "qmra-wiki-sample.xlsx", package = "singlehitapp")
}

# Three Cryptosporidium isolates from the sample workbook, pooled as the
# Pooling tab sends them: the pooled data, the per-experiment components, and
# an analysis of the pooled data.
pooled_crypto <- function(bootstrap = FALSE) {
  rows <- read_qmra_workbook(qmra_sample_path())
  experiments <- library_experiments(rows)
  keys <- experiments$experiment_key[experiments$experiment_id %in% c("140", "141", "183")]
  selection <- pool_selection(rows, experiments, keys)
  datasets <- pool_datasets(rows, selection)
  data <- singlehit::as_dose_response(pool_fit_data(datasets, selection))
  analysis <- suppressWarnings(singlehit::analyze_dose_response(
    data,
    models = c("exponential", "beta_poisson"),
    bootstrap_times = 0L,
    exact_bootstrap_times = 0L,
    check_trend = FALSE
  ))
  if (bootstrap) {
    boots <- suppressWarnings(singlehit::bootstrap_dose_response_models(analysis$fits, times = 20, seed = 1, backend = "sequential"))
    analysis <- singlehit::add_bootstrap(analysis, boots)
  }
  list(data = data, components = pool_components(datasets, selection), analysis = analysis)
}
