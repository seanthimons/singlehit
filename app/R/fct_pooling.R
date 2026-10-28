#' Experiments chosen for a poolability test, with stable labels
#'
#' Experiment ids repeat across pathogens ("1" is both Yersinia pestis and
#' Variola major), so each experiment is labelled by pathogen and id, adding
#' the outcome when that is still ambiguous.
#'
#' @param rows Output of [read_qmra_workbook()].
#' @param experiments Output of [library_experiments()].
#' @param keys `experiment_key` values, in the order chosen.
#' @return The matching rows of `experiments` with `label` and `signature`
#'   (the experiment's dose groups, to spot the same counts entered twice).
#' @noRd
pool_selection <- function(rows, experiments, keys) {
  selection <- experiments[match(unique(keys), experiments$experiment_key), , drop = FALSE]
  selection <- selection[!is.na(selection$experiment_key), , drop = FALSE]
  if (nrow(selection) == 0L) {
    selection$label <- character()
    selection$signature <- character()
    return(selection)
  }
  label <- paste0(selection$pathogen, " #", selection$experiment_id)
  ambiguous <- label %in% label[duplicated(label)]
  label[ambiguous] <- paste0(label[ambiguous], " (", selection$response_type[ambiguous], ")")
  selection$label <- make.unique(label, sep = " ")
  selection$signature <- vapply(selection$experiment_key, function(key) {
    counts <- rows[rows$experiment_key == key & !is.na(rows$dose), c("dose", "positive", "negative"), drop = FALSE]
    if (nrow(counts) == 0L) {
      return(NA_character_)
    }
    counts <- counts[order(counts$dose, counts$positive, counts$negative), , drop = FALSE]
    paste(counts$dose, counts$positive, counts$negative, sep = ":", collapse = ";")
  }, character(1L), USE.NAMES = FALSE)
  rownames(selection) <- NULL
  selection
}

#' @noRd
pool_normalise <- function(x) tolower(trimws(as.character(x)))

#' @noRd
pool_differs <- function(values, what) {
  values <- unique(values)
  if (length(unique(pool_normalise(values))) > 1L) {
    sprintf("The experiments differ in %s: %s.", what, paste(values, collapse = "; "))
  } else {
    character()
  }
}

# PINCH POINT: these rules, and above all their severities, are a first,
# deliberately loose cut and need revisiting (see #20, #21, #35). "block" stops
# the test from running; "warn" is shown and logged, but the test still runs.
# Only data problems and dose units block for now. Outcome, host, route, and
# pathogen differences warn, because the QMRA wiki itself pools across hosts
# and routes (for example "21,23" is guinea pig plus diabetic rat).
# Each check takes a pool_selection() and returns the problems it finds.
pool_rules <- list(
  list(
    id = "size",
    severity = "block",
    label = "At least two experiments",
    check = function(selection) {
      if (nrow(selection) < 2L) "Choose at least two experiments to test." else character()
    }
  ),
  list(
    id = "fittable",
    severity = "block",
    label = "Every experiment can be fitted",
    check = function(selection) {
      bad <- selection[!selection$fittable, , drop = FALSE]
      if (nrow(bad) == 0L) character() else paste0(bad$label, ": ", bad$problem)
    }
  ),
  list(
    id = "wiki_pooled",
    severity = "block",
    label = "No entry the wiki already pooled",
    check = function(selection) {
      bad <- selection$label[selection$wiki_pooled]
      if (length(bad) == 0L) {
        character()
      } else {
        sprintf("%s already pools other experiments, so its subjects would be counted twice.", bad)
      }
    }
  ),
  list(
    id = "duplicate",
    severity = "block",
    label = "No experiment chosen twice",
    check = function(selection) {
      id <- paste(selection$pathogen, selection$experiment_id)
      repeated <- unique(id[duplicated(id)])
      signature <- selection$signature
      copies <- unique(signature[!is.na(signature) & duplicated(signature)])
      c(
        if (length(repeated) > 0L) {
          sprintf("%s appears more than once (under different outcomes or models).", repeated)
        },
        vapply(copies, function(value) {
          sprintf("%s have identical dose-response counts.", paste(selection$label[signature %in% value], collapse = " and "))
        }, character(1L), USE.NAMES = FALSE)
      )
    }
  ),
  list(
    id = "dose_unit",
    severity = "block",
    label = "One reported dose unit",
    check = function(selection) {
      c(
        if (any(selection$dose_unit == "Not reported")) {
          sprintf("No dose unit is reported for %s.", paste(selection$label[selection$dose_unit == "Not reported"], collapse = ", "))
        },
        pool_differs(selection$dose_unit, "dose unit")
      )
    }
  ),
  list(id = "outcome_group", severity = "warn", label = "One outcome group", check = function(selection) pool_differs(selection$outcome_group, "outcome group")),
  list(id = "response_type", severity = "warn", label = "One outcome description", check = function(selection) pool_differs(selection$response_type, "reported outcome")),
  list(id = "host", severity = "warn", label = "One host", check = function(selection) pool_differs(selection$host, "host")),
  list(id = "route", severity = "warn", label = "One exposure route", check = function(selection) pool_differs(selection$route, "exposure route")),
  list(id = "pathogen", severity = "warn", label = "One pathogen", check = function(selection) pool_differs(selection$pathogen, "pathogen"))
)

#' Run every pooling rule against a selection
#'
#' @param selection Output of [pool_selection()].
#' @return One row per rule: `id`, `severity`, `label`, `passed`, and
#'   `message` (the problems found, joined).
#' @noRd
pool_check <- function(selection) {
  out <- lapply(pool_rules, function(rule) {
    problems <- rule$check(selection)
    data.frame(
      id = rule$id,
      severity = rule$severity,
      label = rule$label,
      passed = length(problems) == 0L,
      message = paste(problems, collapse = " "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Whether any blocking rule failed
#' @noRd
pool_blocked <- function(checks) {
  any(!checks$passed & checks$severity == "block")
}

#' Datasets for the pooling functions, named by experiment label
#'
#' Each dataset's `study_id` is its label, so plots keep experiments apart, and
#' every dataset gets the first experiment's spelling of the shared dose unit
#' so the pooled rows carry one unit.
#' @noRd
pool_datasets <- function(rows, selection) {
  datasets <- lapply(seq_len(nrow(selection)), function(i) {
    data <- library_experiment_data(rows, selection$experiment_key[[i]])
    data$study_id <- selection$label[[i]]
    data$dose_unit <- selection$dose_unit[[1L]]
    data
  })
  stats::setNames(datasets, selection$label)
}

#' Run the poolability test, the pooled fit, and the separate fits
#'
#' Warnings from fits that do not converge are collected rather than stopping
#' the run. An error, for example a model with no workable starting value,
#' is returned as the condition.
#'
#' @param datasets Output of [pool_datasets()].
#' @param models Models to test.
#' @param alpha Significance level.
#' @return A list of `test`, `pooled`, `separate` (fits by model, then by
#'   experiment, `NULL` where a fit failed), `datasets`, `models`, `alpha`, and
#'   `warnings`; or an error condition.
#' @noRd
pool_run <- function(datasets, models, alpha = 0.05) {
  warnings <- character()
  collect <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  tryCatch(
    {
      test <- collect(singlehit::poolability_test(datasets, models = models, alpha = alpha))
      pooled <- collect(singlehit::fit_pooled_dose_response(datasets, models = models))
      separate <- lapply(stats::setNames(models, models), function(model) {
        lapply(datasets, function(data) {
          tryCatch(collect(singlehit::fit_dose_response(data, model)), error = function(e) {
            warnings <<- c(warnings, conditionMessage(e))
            NULL
          })
        })
      })
      list(
        test = test,
        pooled = pooled,
        separate = separate,
        datasets = datasets,
        models = models,
        alpha = alpha,
        warnings = unique(warnings)
      )
    },
    error = function(e) e
  )
}

#' The test results as a display table
#'
#' Deliberately avoids saying the experiments "are poolable": with few doses
#' per experiment the test has little power, so not rejecting pooling is not
#' evidence that the experiments share one curve (#20).
#' @noRd
pool_test_table <- function(run) {
  test <- run$test
  data.frame(
    Model = model_label_ui(test$model),
    `Pooled deviance` = format_number(test$deviance_pooled),
    `Separate deviance` = format_number(test$deviance_unpooled),
    `LRT statistic` = format_number(test$lrt_statistic),
    df = test$df,
    `Critical value` = format_number(test$chi_square_critical),
    `p-value` = format_p(test$p_value),
    Result = ifelse(
      test$poolable,
      sprintf("Pooling not rejected at alpha = %s", run$alpha),
      sprintf("Pooling rejected at alpha = %s", run$alpha)
    ),
    Converged = ifelse(test$converged, "yes", "no"),
    check.names = FALSE
  )
}

#' Observed points, separate curves, and pooled curves for plotting
#' @noRd
pool_curves <- function(run, points = 100L) {
  observed <- do.call(rbind, lapply(names(run$datasets), function(label) {
    data <- singlehit::as_dose_response(run$datasets[[label]])
    data.frame(experiment = label, dose = data$dose, response = data$response)
  }))
  curve <- function(fit, label, kind, model) {
    if (is.null(fit)) {
      return(NULL)
    }
    out <- singlehit::prediction_curve(fit, points = points, dose_range = range(fit$data$dose))
    data.frame(experiment = label, kind = kind, model = model_label_ui(model), dose = out$dose, estimate = out$estimate)
  }
  curves <- do.call(rbind, lapply(run$models, function(model) {
    separate <- lapply(names(run$separate[[model]]), function(label) {
      curve(run$separate[[model]][[label]], label, "Separate fit", model)
    })
    rbind(do.call(rbind, separate), curve(run$pooled[[model]], "Pooled", "Pooled fit", model))
  }))
  list(observed = observed, curves = curves)
}

#' Plot each experiment's own fit against the pooled fit, one panel per model
#' @noRd
pool_plot <- function(run) {
  parts <- pool_curves(run)
  separate <- parts$curves[parts$curves$kind == "Separate fit", , drop = FALSE]
  pooled <- parts$curves[parts$curves$kind == "Pooled fit", , drop = FALSE]
  observed <- merge(parts$observed, data.frame(model = unique(parts$curves$model)), by = NULL)
  # Panels in the order the models were chosen, not alphabetically.
  levels <- model_label_ui(run$models)
  separate$model <- factor(separate$model, levels = levels)
  pooled$model <- factor(pooled$model, levels = levels)
  observed$model <- factor(observed$model, levels = levels)
  unit <- run$datasets[[1L]]$dose_unit[[1L]]
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$dose)) +
    ggplot2::geom_line(
      data = separate,
      ggplot2::aes(y = .data$estimate, colour = .data$experiment),
      linewidth = 0.5,
      alpha = 0.8
    ) +
    ggplot2::geom_line(data = pooled, ggplot2::aes(y = .data$estimate), colour = "black", linewidth = 1.1) +
    ggplot2::geom_point(data = observed, ggplot2::aes(y = .data$response, colour = .data$experiment), size = 2) +
    ggplot2::facet_wrap(ggplot2::vars(.data$model)) +
    ggplot2::scale_x_log10(breaks = scales::breaks_log(n = 5), labels = scales::label_log()) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = sprintf("Dose (%s, log10 scale)", unit),
      y = "Proportion responding",
      colour = "Experiment",
      caption = "Coloured: each experiment's own fit and observed proportions. Black: the pooled fit to the stacked dose groups."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical", plot.caption = ggplot2::element_text(hjust = 0))
}

#' Parameter estimates for each experiment and for the pooled fit
#'
#' Wald intervals from [generics::tidy()], which are `NA` when the fit's
#' Hessian is singular (common for beta-Poisson with few doses).
#' @noRd
pool_parameters <- function(run) {
  one <- function(fit, label, model) {
    if (is.null(fit)) {
      return(NULL)
    }
    tidy <- as.data.frame(generics::tidy(fit, conf.int = TRUE))
    data.frame(
      model = model_label_ui(model),
      experiment = label,
      term = tidy$term,
      estimate = tidy$estimate,
      conf.low = tidy$conf.low,
      conf.high = tidy$conf.high
    )
  }
  do.call(rbind, lapply(run$models, function(model) {
    separate <- lapply(names(run$separate[[model]]), function(label) one(run$separate[[model]][[label]], label, model))
    rbind(do.call(rbind, separate), one(run$pooled[[model]], "Pooled", model))
  }))
}

#' Forest plot of parameter estimates, to show spread between experiments
#'
#' A direct view of heterogeneity that does not depend on the test's power:
#' if the experiments' intervals barely overlap, a single pooled curve hides
#' real differences whatever the test says.
#' @noRd
pool_forest <- function(run) {
  parameters <- pool_parameters(run)
  panels <- unique(paste(parameters$model, parameters$term, sep = ": "))
  parameters$panel <- factor(paste(parameters$model, parameters$term, sep = ": "), levels = panels)
  parameters$experiment <- factor(parameters$experiment, levels = rev(c(names(run$datasets), "Pooled")))
  parameters$pooled <- parameters$experiment == "Pooled"
  intervals <- parameters[is.finite(parameters$conf.low) & is.finite(parameters$conf.high), , drop = FALSE]
  reference <- parameters[parameters$pooled, , drop = FALSE]
  ggplot2::ggplot(parameters, ggplot2::aes(x = .data$estimate, y = .data$experiment)) +
    ggplot2::geom_vline(data = reference, ggplot2::aes(xintercept = .data$estimate), linetype = "dashed", colour = "grey40") +
    ggplot2::geom_linerange(
      data = intervals,
      ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high, colour = .data$pooled),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$pooled, shape = .data$pooled), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#0066A1", `TRUE` = "black"), guide = "none") +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
    # Plain numbers: power-of-ten labels read badly over less than a decade.
    ggplot2::scale_x_log10(breaks = scales::breaks_log(n = 4), labels = function(x) format_number(x, digits = 3)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free_x") +
    ggplot2::labs(
      x = "Estimate (log10 scale) with 95% Wald interval",
      y = NULL,
      caption = "Dashed line: pooled estimate. A point without a bar has no interval (the fit's information matrix is singular)."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}

#' Experiment ids listed in a wiki-pooled entry such as "281,282 (pooled)"
#' @noRd
pool_wiki_ids <- function(id) {
  id <- gsub("\\(pooled\\)", "", id, ignore.case = TRUE)
  parts <- trimws(unlist(strsplit(id, ",|&|\\band\\b")))
  parts[nzchar(parts)]
}

#' The wiki's own pooled entry for exactly these experiments, if any
#'
#' Lets a pooled fit be checked against what the wiki reports, for example
#' 21 + 23 for Burkholderia mallei against the wiki's "21,23".
#' @return A display table, or `NULL` when no entry matches.
#' @noRd
pool_wiki_reference <- function(rows, experiments, selection) {
  if (length(unique(selection$pathogen)) != 1L) {
    return(NULL)
  }
  candidates <- experiments[experiments$wiki_pooled & experiments$pathogen == selection$pathogen[[1L]], , drop = FALSE]
  match <- vapply(candidates$experiment_id, function(id) setequal(pool_wiki_ids(id), selection$experiment_id), logical(1L))
  candidates <- candidates[match, , drop = FALSE]
  if (nrow(candidates) == 0L) {
    return(NULL)
  }
  data.frame(
    `Wiki entry` = candidates$experiment_id,
    Outcome = candidates$response_type,
    `Preferred model` = candidates$model_label,
    `Reported parameters` = rows$parameters[match(candidates$experiment_key, rows$experiment_key)],
    check.names = FALSE
  )
}

#' Log rows for one run: one row per model
#' @noRd
pool_log_entry <- function(run, selection, checks, rationale, workbook, time = Sys.time()) {
  warned <- checks$label[!checks$passed & checks$severity == "warn"]
  data.frame(
    time = format(time, "%Y-%m-%d %H:%M:%S"),
    workbook = if (is.null(workbook)) "" else workbook,
    experiments = paste(selection$label, collapse = "; "),
    dose_unit = selection$dose_unit[[1L]],
    rationale = rationale,
    warnings_acknowledged = paste(warned, collapse = "; "),
    model = run$test$model,
    alpha = run$alpha,
    lrt_statistic = run$test$lrt_statistic,
    df = run$test$df,
    p_value = run$test$p_value,
    pooling_rejected = !run$test$poolable,
    converged = run$test$converged,
    stringsAsFactors = FALSE
  )
}
