# Run from the package root, optionally adding --exact for the third curve.
# Rscript data-raw/plot-ward-rotavirus.R [--exact]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || (length(args) == 1L && args[[1L]] != "--exact")) {
  stop("Only --exact is supported.")
}
include_exact <- length(args) == 1L

pkgload::load_all(".", quiet = TRUE)
models <- c("exponential", "beta_poisson")
if (include_exact) models <- c(models, "exact_beta_poisson")

elapsed <- system.time({
  # Plotting needs one fit per model; leave all bootstrap work for another machine.
  analysis <- analyze_dose_response(
    ward_rotavirus,
    models = models,
    bootstrap_times = 0L,
    exact_bootstrap_times = 0L
  )
})[["elapsed"]]
print(analysis$comparison[, c("model", "AIC", "preferred")])

plot <- plot_model_overlay(analysis) +
  ggplot2::labs(
    title = "Rotavirus infection: model comparison",
    x = "Dose (focus-forming units)",
    y = "Probability of infection"
  )
dir.create("dev", showWarnings = FALSE)
output <- if (include_exact) "dev/ward-rotavirus-three-models.png" else "dev/ward-rotavirus-two-models.png"
ggplot2::ggsave(output, plot, width = 7.5, height = 5, dpi = 160)
cat(sprintf("Fit and comparison: %.2f seconds. Saved %s\n", elapsed, output))
