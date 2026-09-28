# Run from the package root:
# Rscript data-raw/plot-preferred-models.R workbook.xlsx overview.png [--facet-outcome]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L || (length(args) == 3L && args[[3L]] != "--facet-outcome")) {
  stop("Supply the workbook and output PNG paths, optionally followed by --facet-outcome.")
}
facet_outcome <- length(args) == 3L

pkgload::load_all(".", quiet = TRUE)
raw <- readxl::read_excel(args[[1L]], sheet = "raw data", .name_repair = "minimal")
key <- paste(raw$experiment_id, raw$pathogen, raw[["Response type"]], raw[["Preferred model"]], sep = "|")
experiments <- split(raw, key)
skipped <- character()
parameter <- function(text, name) {
  pattern <- paste0(name, "[^=]*=\\s*([0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?)")
  match <- regmatches(text, regexec(pattern, text, ignore.case = TRUE, perl = TRUE))[[1L]]
  if (length(match) < 2L) NA_real_ else as.numeric(match[[2L]])
}

curves <- purrr::map_dfr(names(experiments), function(study_key) {
  experiment <- experiments[[study_key]]
  preferred <- tolower(trimws(experiment[["Preferred model"]][[1L]]))
  model <- switch(preferred, "exponential" = "exponential", "beta-poisson" = "beta_poisson", NULL)
  if (is.null(model)) {
    skipped[[study_key]] <<- paste("unsupported model:", preferred)
    return(NULL)
  }

  data <- data.frame(
    dose = suppressWarnings(as.numeric(experiment$Dose)),
    positive = suppressWarnings(as.numeric(experiment$Positive)),
    negative = suppressWarnings(as.numeric(experiment$Negative))
  )
  data <- stats::na.omit(data)
  fit <- tryCatch(fit_dose_response(data, model), error = function(e) e)
  if (inherits(fit, "error") || fit$convergence != 0L) {
    parameters <- experiment[["Optimized parameters"]][[1L]]
    k <- parameter(parameters, "k")
    alpha <- parameter(parameters, "(?:a|α)")
    n50 <- parameter(parameters, "N50")
    median_dose <- if (model == "exponential") log(2) / k else n50
    if (!is.finite(median_dose) || median_dose <= 0 || (model == "beta_poisson" && (!is.finite(alpha) || alpha <= 0))) {
      skipped[[study_key]] <<- "no usable fit or workbook parameters"
      return(NULL)
    }
    # ponytail: parameter-only curves use a 100x ID50 window; use study dose ranges if recovered.
    dose <- exp(seq(log(median_dose / 100), log(median_dose * 100), length.out = 100L))
    estimate <- if (model == "exponential") exponential_response(dose, k) else beta_poisson_response(dose, alpha, n50)
    return(tibble::tibble(
      dose = dose,
      estimate = estimate,
      study_key = study_key,
      group = tools::toTitleCase(tolower(experiment$pathogen_category[[1L]])),
      outcome = experiment[["Response type"]][[1L]],
      source = "Workbook parameters"
    ))
  }

  range <- range(fit$data$dose)
  prediction_curve(fit, points = 100L, dose_range = range) |>
    dplyr::mutate(
      study_key = study_key,
      group = tools::toTitleCase(tolower(experiment$pathogen_category[[1L]])),
      outcome = experiment[["Response type"]][[1L]],
      source = "Refit from counts"
    )
})

if (nrow(curves) == 0L) stop("No preferred models could be plotted.")
if (any(!is.finite(curves$estimate))) stop("A curve contains invalid probabilities.")
outcome <- tolower(curves$outcome)
curves$outcome_group <- ifelse(
  grepl("death|mortality|stillbirth", outcome),
  "Death or mortality",
  ifelse(grepl("infection|culture|isolation|shedding|invasion", outcome), "Infection or detection", "Illness or clinical signs")
)
curves$outcome_group <- factor(curves$outcome_group, levels = c("Infection or detection", "Illness or clinical signs", "Death or mortality"))
groups <- sort(unique(curves$group))
colors <- c(Bacteria = "#C65D00", Virus = "#0066A1", Protozoa = "#008C6A", Prion = "#6A1B9A")
counts <- table(curves$group[!duplicated(curves$study_key)])
labels <- paste0(groups, " (", counts[groups], ")")
names(labels) <- groups

plot <- ggplot2::ggplot(curves, ggplot2::aes(x = .data$dose, y = .data$estimate, group = .data$study_key, color = .data$group)) +
  ggplot2::geom_line(alpha = 0.55, linewidth = 0.55) +
  ggplot2::scale_color_manual(values = colors, breaks = groups, labels = labels) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
  ggplot2::scale_x_log10(labels = scales::label_log(base = 10)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    title = "Preferred dose-response models across pathogens",
    subtitle = sprintf("%d experiments with preferred exponential or beta-Poisson models", length(unique(curves$study_key))),
    x = "Recorded dose (log10 scale; study-specific units)",
    y = "Probability of reported outcome",
    color = "Pathogen group",
    caption = "Curves use observed dose ranges where available; otherwise 0.01–100 × ID50. Dose units and outcomes vary."
  ) +
  ggplot2::theme_minimal(base_size = 11)
if (facet_outcome) plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$outcome_group), ncol = 1)

ggplot2::ggsave(args[[2L]], plot, width = 10, height = if (facet_outcome) 11 else 6, dpi = 160)
cat(sprintf("Plotted %d of %d experiments.\n", length(unique(curves$study_key)), length(experiments)))
print(table(curves$source[!duplicated(curves$study_key)]))
print(table(curves$outcome_group[!duplicated(curves$study_key)]))
if (length(skipped)) print(table(skipped))
