#' Columns the Library tab needs from the workbook's "raw data" sheet
#' @noRd
library_required_columns <- c(
  "pathogen_category", "pathogen", "experiment_id", "Exposure route", "Dose Units",
  "Host type", "Preferred model", "Optimized parameters", "Response type",
  "Dose", "Positive", "Negative"
)

#' Fields offered as facets, colours, and filters on the Library tab
#' @noRd
library_fields <- c(
  "Pathogen group" = "category",
  "Outcome group" = "outcome_group",
  "Dose unit" = "dose_unit",
  "Host" = "host_group",
  "Exposure route" = "route_group",
  "Strain or isolate" = "strain",
  "Preferred model" = "model_label"
)

#' Read the QMRA wiki workbook into one row per dose group
#'
#' Reads the "raw data" sheet and adds normalised metadata so that free-text
#' spellings ("mice", "C57BL/6 mice") fall into the same group. Experiments are
#' keyed as in `data-raw/plot-preferred-models.R`: the experiment id alone is
#' not unique, because the same id can be reported for several outcomes.
#'
#' @param path Path to an `.xlsx` workbook.
#' @return A data frame with the original metadata, numeric `dose`, `positive`,
#'   and `negative` (`NA` in the single row kept for an experiment that reports
#'   only parameters), and the normalised columns `experiment_key`, `category`,
#'   `outcome_group`, `dose_unit`, `host_group`, `route_group`, `strain`, `model`,
#'   `model_label`, and `wiki_pooled`.
#' @noRd
read_qmra_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if (!"raw data" %in% sheets) {
    stop("The workbook has no \"raw data\" sheet.", call. = FALSE)
  }
  # "minimal": the sheet repeats one percentile column name, which is not used.
  raw <- readxl::read_excel(path, sheet = "raw data", .name_repair = "minimal")
  missing <- setdiff(library_required_columns, names(raw))
  if (length(missing) > 0L) {
    stop(sprintf("The \"raw data\" sheet is missing: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }
  # Optional: the agent strain or isolate, often what separates experiments
  # on the same pathogen (Cryptosporidium Iowa, TAMU, and UCP isolates).
  strain <- if ("Agent Strain" %in% names(raw)) raw[["Agent Strain"]] else rep(NA_character_, nrow(raw))
  raw <- as.data.frame(raw[library_required_columns], stringsAsFactors = FALSE)
  rows <- data.frame(
    experiment_id = trimws(as.character(raw$experiment_id)),
    pathogen = trimws(as.character(raw$pathogen)),
    category = library_category(raw$pathogen_category),
    response_type = trimws(as.character(raw[["Response type"]])),
    outcome_group = library_outcome_group(raw[["Response type"]]),
    dose_unit = library_dose_unit(raw[["Dose Units"]]),
    host = trimws(as.character(raw[["Host type"]])),
    host_group = library_host_group(raw[["Host type"]]),
    route = trimws(as.character(raw[["Exposure route"]])),
    route_group = library_route_group(raw[["Exposure route"]]),
    strain = library_strain(strain),
    preferred_model = trimws(as.character(raw[["Preferred model"]])),
    model = library_model(raw[["Preferred model"]]),
    parameters = as.character(raw[["Optimized parameters"]]),
    dose = suppressWarnings(as.numeric(raw$Dose)),
    positive = suppressWarnings(as.numeric(raw$Positive)),
    negative = suppressWarnings(as.numeric(raw$Negative)),
    stringsAsFactors = FALSE
  )
  rows <- rows[!is.na(rows$experiment_id) & nzchar(rows$experiment_id), , drop = FALSE]
  rows$experiment_key <- paste(rows$experiment_id, rows$pathogen, rows$response_type, rows$preferred_model, sep = "|")
  # Some experiments record only the fitted parameters ("NR" counts). Keep one
  # row for each of them so their workbook curve can still be drawn.
  counted <- stats::complete.cases(rows[c("dose", "positive", "negative")])
  parameter_only <- !rows$experiment_key %in% rows$experiment_key[counted] & !duplicated(rows$experiment_key)
  rows <- rows[counted | parameter_only, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop("The \"raw data\" sheet has no experiments.", call. = FALSE)
  }
  rows$model_label <- ifelse(is.na(rows$model), "Other or not reported", model_label_ui(rows$model))
  # Entries such as "142, 143, 144", "297 & 298", or "281,282 (pooled)" were
  # already pooled by the wiki.
  rows$wiki_pooled <- grepl(",|&|\\band\\b|pooled", rows$experiment_id, ignore.case = TRUE)
  rownames(rows) <- NULL
  rows
}

#' @noRd
library_category <- function(x) {
  x <- trimws(as.character(x))
  out <- tools::toTitleCase(tolower(x))
  out[is.na(x) | !nzchar(x)] <- "Not reported"
  out
}

#' Collapse free-text response types into three outcome groups
#'
#' Same rules as `data-raw/plot-preferred-models.R`.
#' @noRd
library_outcome_group <- function(x) {
  x <- tolower(as.character(x))
  out <- ifelse(
    grepl("death|mortality|stillbirth", x),
    "Death or mortality",
    ifelse(grepl("infection|culture|isolation|shedding|invasion", x), "Infection or detection", "Illness or clinical signs")
  )
  out[is.na(x)] <- "Illness or clinical signs"
  out
}

#' @noRd
library_dose_unit <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x) | toupper(x) == "NR"] <- "Not reported"
  x
}

#' @noRd
library_strain <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x) | toupper(x) == "NR"] <- "Not reported"
  x
}

#' @noRd
library_host_group <- function(x) {
  x <- tolower(as.character(x))
  out <- ifelse(
    grepl("human|volunteer", x),
    "Human",
    ifelse(grepl("mice|mouse", x), "Mouse", "Other animal")
  )
  out[is.na(x) | !nzchar(trimws(x))] <- "Not reported"
  out
}

#' @noRd
library_route_group <- function(x) {
  x <- tolower(as.character(x))
  out <- ifelse(
    grepl("oral|intragastric|ingest", x),
    "Oral",
    ifelse(
      grepl("nasal|inhal|aerosol|trache|respir", x),
      "Respiratory",
      ifelse(grepl("intraperitoneal|intravenous|inject|subcutan|intramuscular|intradermal", x), "Injection", "Other")
    )
  )
  out[is.na(x) | !nzchar(trimws(x))] <- "Not reported"
  out
}

#' Map the workbook's preferred model to a singlehit model name
#' @return `"exponential"`, `"beta_poisson"`, or `NA`.
#' @noRd
library_model <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[x %in% "exponential"] <- "exponential"
  out[x %in% "beta-poisson"] <- "beta_poisson"
  out
}

#' Dose groups for one experiment, with the metadata singlehit carries
#'
#' @param rows Output of [read_qmra_workbook()].
#' @param key One `experiment_key`.
#' @return A data frame accepted by [singlehit::as_dose_response()].
#' @noRd
library_experiment_data <- function(rows, key) {
  experiment <- rows[rows$experiment_key == key, , drop = FALSE]
  if (nrow(experiment) == 0L) {
    stop("That experiment is not in the workbook.", call. = FALSE)
  }
  if (anyNA(experiment$dose)) {
    stop("The workbook reports only fitted parameters for this experiment, not dose-response counts.", call. = FALSE)
  }
  data.frame(
    dose = experiment$dose,
    positive = experiment$positive,
    negative = experiment$negative,
    study_id = experiment$experiment_id[[1L]],
    host = library_or_unknown(experiment$host[[1L]]),
    dose_unit = experiment$dose_unit[[1L]],
    endpoint = library_or_unknown(experiment$response_type[[1L]]),
    stringsAsFactors = FALSE
  )
}

#' @noRd
library_or_unknown <- function(x) {
  if (is.na(x) || !nzchar(x)) "Not reported" else x
}

#' One row per experiment, with whether singlehit can fit it
#'
#' @param rows Output of [read_qmra_workbook()].
#' @return A data frame keyed by `experiment_key`. `fittable` is `FALSE`, and
#'   `problem` says why, when [singlehit::as_dose_response()] rejects the data.
#' @noRd
library_experiments <- function(rows) {
  keys <- unique(rows$experiment_key)
  first <- rows[match(keys, rows$experiment_key), , drop = FALSE]
  problem <- vapply(keys, function(key) {
    checked <- tryCatch(singlehit::as_dose_response(library_experiment_data(rows, key)), error = function(e) e)
    if (inherits(checked, "error")) conditionMessage(checked) else NA_character_
  }, character(1L), USE.NAMES = FALSE)
  out <- data.frame(
    experiment_key = keys,
    experiment_id = first$experiment_id,
    pathogen = first$pathogen,
    category = first$category,
    response_type = first$response_type,
    outcome_group = first$outcome_group,
    dose_unit = first$dose_unit,
    host = first$host,
    host_group = first$host_group,
    route = first$route,
    route_group = first$route_group,
    strain = first$strain,
    model = first$model,
    model_label = first$model_label,
    wiki_pooled = first$wiki_pooled,
    doses = as.integer(tapply(!is.na(rows$dose), rows$experiment_key, sum)[keys]),
    subjects = as.numeric(tapply(rows$positive + rows$negative, rows$experiment_key, sum, na.rm = TRUE)[keys]),
    fittable = is.na(problem),
    problem = problem,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Readable label for an experiment in a select input
#' @noRd
library_experiment_label <- function(experiments) {
  paste0(
    experiments$pathogen, " (", experiments$response_type, ", ", experiments$dose_unit,
    "; experiment ", experiments$experiment_id, ")"
  )
}

#' Pull a numeric parameter such as `k = 4.19E-03` out of free text
#' @noRd
library_parameter <- function(text, name) {
  if (is.na(text)) {
    return(NA_real_)
  }
  pattern <- paste0(name, "[^=]*=\\s*([0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?)")
  match <- regmatches(text, regexec(pattern, text, ignore.case = TRUE, perl = TRUE))[[1L]]
  if (length(match) < 2L) NA_real_ else as.numeric(match[[2L]])
}

#' Preferred-model curves for every experiment in the workbook
#'
#' Refits each experiment's preferred model from its counts and draws it over
#' the observed dose range. When the refit fails, the curve comes from the
#' workbook's own parameters over 0.01 to 100 times the ID50, as in
#' `data-raw/plot-preferred-models.R`. Experiments whose preferred model is not
#' exponential or beta-Poisson, or that have neither, are skipped.
#'
#' @param rows Output of [read_qmra_workbook()].
#' @param points Points per curve.
#' @return A data frame of `experiment_key`, `dose`, `estimate`, and `source`,
#'   with the skipped keys and reasons as the `"skipped"` attribute.
#' @noRd
library_curves <- function(rows, points = 100L) {
  keys <- unique(rows$experiment_key)
  skipped <- character()
  curves <- lapply(keys, function(key) {
    experiment <- rows[rows$experiment_key == key, , drop = FALSE]
    model <- experiment$model[[1L]]
    if (is.na(model)) {
      skipped[[key]] <<- paste("unsupported model:", experiment$preferred_model[[1L]])
      return(NULL)
    }
    fit <- tryCatch(
      singlehit::fit_dose_response(library_experiment_data(rows, key)[c("dose", "positive", "negative")], model),
      error = function(e) e
    )
    if (!inherits(fit, "error") && identical(fit$convergence, 0L)) {
      curve <- singlehit::prediction_curve(fit, points = points, dose_range = range(fit$data$dose))
      return(data.frame(experiment_key = key, dose = curve$dose, estimate = curve$estimate, source = "Refit from counts"))
    }
    text <- experiment$parameters[[1L]]
    k <- library_parameter(text, "k")
    alpha <- library_parameter(text, "(?:a|\u03b1)")
    n50 <- library_parameter(text, "N50")
    median_dose <- if (model == "exponential") log(2) / k else n50
    if (!is.finite(median_dose) || median_dose <= 0 || (model == "beta_poisson" && (!is.finite(alpha) || alpha <= 0))) {
      skipped[[key]] <<- "no usable fit or workbook parameters"
      return(NULL)
    }
    dose <- exp(seq(log(median_dose / 100), log(median_dose * 100), length.out = points))
    estimate <- if (model == "exponential") {
      singlehit::exponential_response(dose, k)
    } else {
      singlehit::beta_poisson_response(dose, alpha, n50)
    }
    data.frame(experiment_key = key, dose = dose, estimate = estimate, source = "Workbook parameters")
  })
  out <- do.call(rbind, curves)
  if (is.null(out)) {
    out <- data.frame(experiment_key = character(), dose = numeric(), estimate = numeric(), source = character())
  }
  attr(out, "skipped") <- skipped
  out
}

#' Colours for the pathogen groups, with a fallback for anything else
#' @noRd
library_palette <- function(values) {
  known <- c(Bacteria = "#C65D00", Virus = "#0066A1", Protozoa = "#008C6A", Prion = "#6A1B9A")
  values <- sort(unique(values))
  extra <- setdiff(values, names(known))
  fallback <- grDevices::hcl.colors(max(length(extra), 1L), "Dark 3")[seq_along(extra)]
  c(known, stats::setNames(fallback, extra))[values]
}

#' Overview plot of preferred-model curves
#'
#' @param curves Output of [library_curves()].
#' @param experiments Output of [library_experiments()], already filtered.
#' @param facet One of [library_fields] or `"none"`.
#' @param colour One of [library_fields].
#' @param free_x Whether each facet gets its own dose axis.
#' @return A ggplot object.
#' @importFrom ggplot2 .data
#' @noRd
library_plot <- function(curves, experiments, facet = "none", colour = "category", free_x = FALSE) {
  if (!colour %in% library_fields) {
    stop("Choose a field to colour by.", call. = FALSE)
  }
  if (!facet %in% c("none", library_fields)) {
    stop("Choose a field to facet by.", call. = FALSE)
  }
  metadata <- experiments[c("experiment_key", unique(library_fields))]
  data <- merge(curves, metadata, by = "experiment_key")
  if (nrow(data) == 0L) {
    stop("No curves to plot for these filters.", call. = FALSE)
  }
  data$colour <- data[[colour]]
  counts <- table(data$colour[!duplicated(data$experiment_key)])
  groups <- names(counts)
  labels <- stats::setNames(paste0(groups, " (", as.integer(counts), ")"), groups)
  colours <- if (colour == "category") library_palette(groups) else NULL
  colour_scale <- if (is.null(colours)) {
    ggplot2::scale_colour_discrete(labels = labels)
  } else {
    ggplot2::scale_colour_manual(values = colours, breaks = groups, labels = labels)
  }

  same_units <- facet == "dose_unit" || length(unique(data$dose_unit)) == 1L
  x_label <- if (same_units && length(unique(data$dose_unit)) == 1L) {
    sprintf("Dose (%s, log10 scale)", data$dose_unit[[1L]])
  } else if (same_units) {
    "Dose (log10 scale; unit in panel title)"
  } else {
    "Recorded dose (log10 scale; study-specific units)"
  }
  caption <- paste(
    "Curves use observed dose ranges when refit from counts; otherwise 0.01-100 x ID50 from workbook parameters (dashed).",
    if (same_units) "" else "Dose units differ between curves, so horizontal positions are not comparable; facet by dose unit to compare."
  )

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$dose,
      y = .data$estimate,
      group = .data$experiment_key,
      colour = .data$colour,
      linetype = .data$source
    )
  ) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.55) +
    colour_scale +
    ggplot2::scale_linetype_manual(values = c("Refit from counts" = "solid", "Workbook parameters" = "dashed")) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    # Powers of ten ("10^5") stay short enough not to collide in narrow facets.
    ggplot2::scale_x_log10(breaks = scales::breaks_log(n = 4), labels = scales::label_log()) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      subtitle = sprintf("%d experiments", length(unique(data$experiment_key))),
      x = x_label,
      y = "Probability of reported outcome",
      colour = names(library_fields)[library_fields == colour][[1L]],
      linetype = "Curve source",
      caption = paste(strwrap(caption, width = 90), collapse = "\n")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right", plot.caption = ggplot2::element_text(hjust = 0))
  if (facet != "none") {
    plot <- plot + ggplot2::facet_wrap(
      ggplot2::vars(.data[[facet]]),
      scales = if (free_x) "free_x" else "fixed",
      labeller = ggplot2::label_wrap_gen(width = 20)
    )
  }
  plot
}
