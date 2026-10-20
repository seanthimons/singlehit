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
