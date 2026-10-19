#' Display label for a singlehit model name
#'
#' @param model Character vector of model names such as `"beta_poisson"`.
#' @return Character vector of display labels; unknown names pass through.
#' @noRd
model_label_ui <- function(model) {
  labels <- c(
    exponential = "Exponential",
    beta_poisson = "Beta-Poisson",
    exact_beta_poisson = "Exact beta-Poisson"
  )
  out <- unname(labels[model])
  out[is.na(out)] <- model[is.na(out)]
  out
}

#' Format p-values for display
#'
#' @param p Numeric vector of p-values.
#' @param digits Significant digits for values at or above 0.001.
#' @noRd
format_p <- function(p, digits = 3) {
  out <- rep("n/a", length(p))
  known <- !is.na(p)
  out[known] <- ifelse(p[known] < 0.001, "<0.001", trimws(formatC(p[known], digits = digits, format = "g")))
  out
}

#' Format a numeric column for display, leaving missing values as "n/a"
#' @noRd
format_number <- function(x, digits = 4) {
  out <- rep("n/a", length(x))
  known <- !is.na(x)
  out[known] <- trimws(formatC(x[known], digits = digits, format = "g"))
  out
}

#' Display label for an assessment recommendation code
#' @noRd
recommendation_label <- function(recommendation) {
  labels <- c(
    recommended = "Recommended",
    acceptable_alternative = "Acceptable alternative",
    preferred_but_inadequate = "Preferred, but fit is inadequate",
    not_recommended = "Not recommended"
  )
  out <- unname(labels[recommendation])
  out[is.na(out)] <- recommendation[is.na(out)]
  out
}

#' Display label for a consensus agreement level
#' @noRd
agreement_label <- function(agreement) {
  labels <- c(unanimous = "Unanimous", majority = "Majority", no_consensus = "No consensus")
  out <- unname(labels[agreement])
  out[is.na(out)] <- agreement[is.na(out)]
  out
}

#' Evaluate an expression, collecting its warnings and any error
#'
#' Warnings are muffled and returned instead of printed. Warnings raised before
#' an error are kept.
#'
#' @param expr Expression to evaluate.
#' @return A list with `value` (`NULL` on error), `error` (message or `NULL`),
#'   and `warnings` (unique messages).
#' @noRd
with_captured_warnings <- function(expr) {
  warnings <- character()
  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  failed <- inherits(result, "error")
  list(
    value = if (failed) NULL else result,
    error = if (failed) conditionMessage(result) else NULL,
    warnings = unique(warnings)
  )
}

#' Run the fast (no-bootstrap) singlehit analysis and build display tables
#'
#' Everything that can warn runs here so warnings are captured in one place.
#'
#' @param data Standardized dose-response data.
#' @param models Model names to fit; at least two.
#' @param alpha Significance level.
#' @return A list from `with_captured_warnings()` whose `value` holds
#'   `analysis`, `parameters`, and `fit_stats`.
#' @noRd
run_fast_analysis <- function(data, models, alpha) {
  with_captured_warnings({
    analysis <- singlehit::analyze_dose_response(
      data,
      models = models,
      bootstrap_times = 0L,
      exact_bootstrap_times = 0L,
      alpha = alpha,
      check_trend = FALSE
    )
    list(
      analysis = analysis,
      parameters = parameter_table(analysis),
      fit_stats = fit_stats_table(analysis)
    )
  })
}

#' Parameter estimates with 95% Wald intervals, one block per model
#' @noRd
parameter_table <- function(analysis) {
  rows <- lapply(analysis$fits, function(fit) {
    tidy <- generics::tidy(fit, conf.int = TRUE)
    data.frame(
      Model = model_label_ui(fit$model),
      Parameter = tidy$term,
      Estimate = format_number(tidy$estimate),
      `Std. error` = format_number(tidy$std.error),
      `95% CI` = paste0("(", format_number(tidy$conf.low), ", ", format_number(tidy$conf.high), ")"),
      check.names = FALSE
    )
  })
  do.call(rbind, unname(rows))
}

#' Deviance, goodness of fit, and information criteria, one row per model
#' @noRd
fit_stats_table <- function(analysis) {
  glance <- do.call(rbind, unname(lapply(analysis$fits, function(fit) as.data.frame(generics::glance(fit)))))
  gof <- as.data.frame(analysis$goodness_of_fit)
  merged <- merge(glance, gof[c("model", "p_value", "assessment")], by = "model", sort = FALSE)
  data.frame(
    Model = model_label_ui(merged$model),
    Deviance = format_number(merged$deviance),
    `Residual df` = merged$df.residual,
    `Goodness-of-fit p` = format_p(merged$p_value),
    `Fit` = ifelse(merged$assessment == "adequate", "Adequate", "Inadequate"),
    AIC = format_number(merged$AIC),
    BIC = format_number(merged$BIC),
    Converged = ifelse(merged$convergence == 0L, "yes", "no"),
    check.names = FALSE
  )
}

#' Headline model decision from the assessment table
#'
#' @param assessment The `assessment` tibble of a `qdr_analysis`.
#' @return A list with `status` (`"recommended"` or `"none"`), `model`
#'   (display label or `NA`), and `text`.
#' @noRd
headline_decision <- function(assessment) {
  recommended <- assessment[assessment$recommendation == "recommended", , drop = FALSE]
  if (nrow(recommended) >= 1L) {
    return(list(
      status = "recommended",
      model = model_label_ui(recommended$model[[1L]]),
      text = recommended$conclusion[[1L]]
    ))
  }
  list(
    status = "none",
    model = NA_character_,
    text = paste(
      "No model is recommended: none shows both an adequate absolute fit and preference in the",
      "relative comparison.",
      paste(assessment$conclusion, collapse = " ")
    )
  )
}

#' Assessment table for display
#' @noRd
assessment_table <- function(assessment) {
  data.frame(
    Model = model_label_ui(assessment$model),
    Deviance = format_number(assessment$deviance),
    `Fit p` = format_p(assessment$gof_p_value),
    `Adequate fit` = ifelse(assessment$appropriate, "yes", "no"),
    `Deviance difference` = format_number(assessment$deviance_difference),
    `Comparison p` = format_p(assessment$chi_square_p_value),
    Preferred = ifelse(assessment$preferred, "yes", "no"),
    Recommendation = recommendation_label(assessment$recommendation),
    check.names = FALSE
  )
}

#' Consensus (experimental) table for display
#' @noRd
consensus_table <- function(consensus) {
  mark <- function(x) ifelse(x, "✓", "")
  data.frame(
    Model = model_label_ui(consensus$model),
    `Chi-squared` = mark(consensus$chi_squared_vote),
    AIC = mark(consensus$AIC_vote),
    BIC = mark(consensus$BIC_vote),
    Votes = consensus$votes,
    Agreement = agreement_label(consensus$agreement),
    Selected = mark(consensus$consensus_selected),
    check.names = FALSE
  )
}

#' Footnote explaining n/a entries in the assessment table
#'
#' The deviance comparison uses the exponential model as its nested baseline,
#' so that model's own comparison columns are always n/a, and every model's are
#' n/a when the exponential model was not fitted.
#'
#' @param assessment The `assessment` tibble of a `qdr_analysis`.
#' @return A string, or `NULL` when nothing is n/a.
#' @noRd
assessment_note <- function(assessment) {
  if (!anyNA(assessment$deviance_difference)) {
    return(NULL)
  }
  if ("exponential" %in% assessment$model) {
    "n/a: the exponential model is the baseline for the deviance comparison."
  } else {
    paste(
      "n/a: the deviance comparison uses the exponential model as its baseline,",
      "which was not among the fitted models."
    )
  }
}

#' Data preview for display: whole-number counts, general-format doses
#' @noRd
data_preview <- function(data) {
  preview <- as.data.frame(data)
  count_columns <- intersect(c("positive", "negative", "total"), names(preview))
  preview$dose <- format_number(preview$dose, digits = 6)
  preview[count_columns] <- lapply(preview[count_columns], function(x) format(x, scientific = FALSE, trim = TRUE))
  if ("response" %in% names(preview)) {
    preview$response <- format_number(preview$response, digits = 3)
  }
  preview
}
