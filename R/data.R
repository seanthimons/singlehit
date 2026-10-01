#' Read grouped dose-response data
#'
#' Reads a delimited text file and standardizes its columns for modeling.
#' Common positive-response names (`positive`, `pos`, and `positive_response`)
#' and negative-response names (`negative`, `neg`, and `negative_response`) are
#' detected automatically.
#'
#' @param path Path to a delimited text file.
#' @param delim Delimiter passed to [readr::read_delim()]. `NULL` asks readr to
#'   detect it.
#' @param dose,positive,negative Optional source column names.
#'
#' @return A tibble with columns `dose`, `positive`, `negative`, `total`, and
#'   `response`, followed by any supplied metadata columns.
#' @examples
#' path <- system.file("extdata", "dose-response-example.csv", package = "singlehit")
#' trial <- read_dose_response(path)
#' trial
#' # Identity columns survive import; response is a numeric proportion.
#' @export
read_dose_response <- function(path, delim = NULL, dose = NULL, positive = NULL, negative = NULL) {
  data <- readr::read_delim(
    file = path,
    delim = delim,
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )

  as_dose_response(data, dose = dose, positive = positive, negative = negative)
}

#' Standardize grouped dose-response data
#'
#' Validates grouped binomial counts, combines rows with the same dose, and
#' calculates total subjects and observed response probabilities.
#' Metadata is optional: plain three-column inputs remain supported.
#' Supplied `study_id`, `host`, `dose_unit`, and `endpoint` must each contain
#' one non-missing, non-blank value throughout the input. `study_id` identifies
#' one experiment. Split different experiments into separate data frames before
#' fitting or testing poolability. Other metadata is preserved, but must agree
#' between rows sharing a dose. `total` and numeric `response` are recalculated;
#' use `endpoint` for the measured outcome, such as infection, illness, or death.
#'
#' @inheritParams read_dose_response
#' @param data A data frame containing dose, positive-response count, and
#'   negative-response count columns.
#'
#' @return A tibble with one row per dose and columns `dose`, `positive`,
#'   `negative`, `total`, and numeric `response`, followed by optional metadata.
#' @examples
#' # Plain three-column data still works.
#' counts <- data.frame(dose = c(1, 10, 100), positive = c(1, 5, 9),
#'                      negative = c(9, 5, 1))
#' as_dose_response(counts)
#' # Add optional experiment identity and descriptive metadata.
#' counts$study_id <- "example_trial"
#' counts$host <- "human"
#' counts$dose_unit <- "FFU"
#' counts$endpoint <- "infection"
#' counts$citation <- "Synthetic teaching example"
#' as_dose_response(counts)
#' # Use endpoint for the outcome label, not the calculated response column.
#' @export
as_dose_response <- function(data, dose = NULL, positive = NULL, negative = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  source_names <- names(data)
  normalized_names <- tolower(gsub("[^[:alnum:]]+", "_", source_names))
  dose <- dose %||% detect_column(source_names, normalized_names, c("dose"), "dose")
  positive <- positive %||%
    detect_column(
      source_names,
      normalized_names,
      c("positive", "pos", "positive_response"),
      "positive response"
    )
  negative <- negative %||%
    detect_column(
      source_names,
      normalized_names,
      c("negative", "neg", "negative_response"),
      "negative response"
    )

  selected <- c(dose, positive, negative)
  if (anyDuplicated(selected)) {
    stop("Dose, positive, and negative columns must be distinct.", call. = FALSE)
  }
  missing_columns <- setdiff(selected, source_names)
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Column%s not found: %s.",
        if (length(missing_columns) == 1L) "" else "s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  standardized <- tibble::tibble(
    dose = data[[dose]],
    positive = data[[positive]],
    negative = data[[negative]]
  )
  validate_dose_response_columns(standardized)

  metadata_names <- setdiff(source_names, c(selected, "total", "response"))
  if (any(metadata_names %in% c("dose", "positive", "negative"))) {
    stop("Metadata cannot use the reserved column names `dose`, `positive`, or `negative` when mapping other count columns.", call. = FALSE)
  }
  metadata <- tibble::as_tibble(data[metadata_names])
  for (column in intersect(metadata_names, c("study_id", "host", "dose_unit", "endpoint"))) {
    value <- metadata[[column]]
    if (anyNA(value) || any(!nzchar(trimws(as.character(value)))) || dplyr::n_distinct(value) != 1L) {
      stop(
        sprintf("`%s` must contain one non-blank value per dataset. Split different experiments, hosts, units, or endpoints into separate data frames.", column),
        call. = FALSE
      )
    }
  }
  standardized <- dplyr::bind_cols(standardized, metadata)
  conflicts <- standardized %>%
    dplyr::group_by(.data$dose) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(metadata_names), dplyr::n_distinct), .groups = "drop")
  for (column in metadata_names) {
    if (any(conflicts[[column]] > 1L)) {
      stop(sprintf("Metadata column `%s` disagrees between rows sharing a dose; resolve the values or split the datasets before coercion.", column), call. = FALSE)
    }
  }

  grouped <- standardized %>%
    dplyr::group_by(.data$dose) %>%
    dplyr::summarise(
      positive = sum(.data$positive),
      negative = sum(.data$negative),
      dplyr::across(dplyr::all_of(metadata_names), dplyr::first),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      total = .data$positive + .data$negative,
      response = .data$positive / .data$total
    ) %>%
    dplyr::arrange(.data$dose) %>%
    dplyr::select(dplyr::all_of(c("dose", "positive", "negative", "total", "response", metadata_names)))
  validate_dose_response_groups(grouped)
  grouped
}

detect_column <- function(source_names, normalized_names, candidates, label) {
  match_index <- match(candidates, normalized_names, nomatch = 0L)
  match_index <- match_index[match_index > 0L]
  if (length(match_index) == 0L) {
    stop(
      sprintf("Could not detect the %s column; supply its name explicitly.", label),
      call. = FALSE
    )
  }
  source_names[[match_index[[1L]]]]
}

validate_dose_response_columns <- function(data) {
  for (column in names(data)) {
    value <- data[[column]]
    if (!is.numeric(value) || any(!is.finite(value))) {
      stop(sprintf("`%s` must contain only finite numeric values.", column), call. = FALSE)
    }
  }
  if (any(data$dose <= 0)) {
    stop("`dose` values must be greater than zero.", call. = FALSE)
  }
  counts <- c(data$positive, data$negative)
  if (any(counts < 0) || any(counts != floor(counts))) {
    stop("Response counts must be non-negative whole numbers.", call. = FALSE)
  }
  if (any(data$positive + data$negative == 0)) {
    stop("Every dose group must contain at least one observation.", call. = FALSE)
  }
  invisible(data)
}

# Fitting criteria evaluated on the aggregated dose groups (post dedup-by-dose):
# at least three distinct doses (the minimum leaving one residual df for a
# two-parameter fit) and more than one positive response in total (a response
# signal that can identify the model). Both are hard errors so non-conforming
# data is rejected before fitting, with a message explaining why.
validate_dose_response_groups <- function(data) {
  if (nrow(data) < 3L) {
    stop(
      sprintf(
        "Data do not meet fitting criteria: at least three distinct dose groups are required, but %d %s present. Three doses is the minimum for a testable two-parameter fit.",
        nrow(data),
        if (nrow(data) == 1L) "is" else "are"
      ),
      call. = FALSE
    )
  }
  if (sum(data$positive) <= 1L) {
    stop(
      sprintf(
        "Data do not meet fitting criteria: more than one positive response is required to identify a dose-response model, but only %d %s observed across all dose groups.",
        sum(data$positive),
        if (sum(data$positive) == 1L) "was" else "were"
      ),
      call. = FALSE
    )
  }
  invisible(data)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
