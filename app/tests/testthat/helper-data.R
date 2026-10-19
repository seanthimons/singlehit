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
