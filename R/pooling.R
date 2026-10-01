#' Test whether dose-response datasets are statistically poolable
#'
#' Runs the poolability likelihood-ratio test (Haas, Rose & Gerba): for each
#' model, fit every dataset separately (unpooled) and the stacked combination
#' (pooled), then compare the deviance difference to a chi-squared distribution.
#' Datasets are poolable when the pooled model does not fit significantly worse
#' than the separate models, i.e. one shared parameter set is adequate.
#'
#' Datasets are combined by stacking their rows (each dataset's dose groups are
#' kept as separate binomial observations); they are not aggregated, so repeated
#' doses across datasets are preserved.
#'
#' @param datasets A named list of dose-response data frames (accepted by
#'   [as_dose_response()]) or already-standardized tibbles. Unnamed elements are
#'   named `dataset_1`, `dataset_2`, and so on.
#' @param models Character vector of models, a subset of `"exponential"`,
#'   `"beta_poisson"`, and `"exact_beta_poisson"`.
#' @param alpha Significance level for the chi-squared test.
#'
#' @return A tibble with one row per model: `model`, `n_datasets`,
#'   `deviance_pooled`, `deviance_unpooled`, `lrt_statistic`, `df`,
#'   `chi_square_critical`, `p_value`, `poolable`, `converged`, and a
#'   human-readable `conclusion`.
#' @details Check that hosts, dose units, and endpoints match before testing.
#' The current pooling functions do not enforce metadata compatibility.
#' `poolable = TRUE` means the test did not detect a significantly worse pooled
#' fit at `alpha`; it does not prove equivalence or adequate absolute fit.
#' Inspect `converged` before interpreting the result.
#' @examples
#' path <- system.file("extdata", "pooling-example.csv", package = "singlehit")
#' raw <- readr::read_csv(path, show_col_types = FALSE)
#' trials <- split(raw, raw$study_id)
#' result <- poolability_test(trials[c("A", "B")])
#' result[c("model", "p_value", "poolable", "converged", "conclusion")]
#' # A and B pass under both models. This test covers only the supplied set.
#' @export
poolability_test <- function(datasets, models = c("exponential", "beta_poisson"), alpha = 0.05) {
  standardized <- standardize_datasets(datasets)
  if (length(standardized) < 2L) {
    stop("`datasets` must contain at least two datasets to test for pooling.", call. = FALSE)
  }
  models <- validate_model_set(models)
  validate_alpha(alpha)

  m <- length(standardized)
  result <- purrr::map_dfr(models, function(model) {
    k <- length(model_parameters(model))
    fits <- lapply(standardized, function(dataset) fit_core(dataset, model))
    pooled <- fit_core(dplyr::bind_rows(standardized), model)
    deviance_unpooled <- sum(vapply(fits, function(fit) fit$deviance, numeric(1)))
    lrt <- max(pooled$deviance - deviance_unpooled, 0)
    df <- k * (m - 1L)
    critical <- stats::qchisq(1 - alpha, df = df)
    poolable <- lrt < critical
    converged <- pooled$convergence == 0L &&
      all(vapply(fits, function(fit) fit$convergence == 0L, logical(1)))
    tibble::tibble(
      model = model,
      n_datasets = m,
      deviance_pooled = pooled$deviance,
      deviance_unpooled = deviance_unpooled,
      lrt_statistic = lrt,
      df = df,
      chi_square_critical = critical,
      p_value = stats::pchisq(lrt, df = df, lower.tail = FALSE),
      poolable = poolable,
      converged = converged,
      conclusion = pool_conclusion(model, poolable)
    )
  })

  if (!all(result$converged)) {
    warning(
      sprintf(
        "Some model fits did not converge; poolability may be unreliable: %s.",
        paste(vapply(result$model[!result$converged], model_label, character(1)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  result
}

#' Report poolability for every combination of datasets
#'
#' Tests every subset containing at least two datasets, separately for each
#' model. For three datasets A, B, and C, reports A+B, A+C, B+C, and A+B+C.
#' Singletons need no pooling test. Results describe candidate combinations;
#' overlapping combinations are allowed and no final grouping is selected.
#' These are exploratory, unadjusted tests. A passing test does not establish
#' equivalence, and results with `converged = FALSE` may be unreliable.
#'
#' @inheritParams poolability_test
#' @param max_datasets Maximum collection size, default six. The number of
#'   combinations grows exponentially (`2^n - n - 1`).
#' @return A tibble with `combination` (a readable label), `datasets` (a list
#'   column of exact dataset names), and all columns from [poolability_test()].
#'   Each row describes one combination under one model.
#' @details A passing pair need not remain poolable after another dataset is
#' added. Inspect the row for the complete combination you intend to pool.
#' Check metadata compatibility before testing; it is not enforced here.
#' Use [group_datasets()] to choose non-overlapping groups from these trials.
#' @examples
#' path <- system.file("extdata", "pooling-example.csv", package = "singlehit")
#' raw <- readr::read_csv(path, show_col_types = FALSE)
#' trials <- split(raw, raw$study_id)
#' report <- poolability_combinations(trials)
#' report[c("combination", "model", "p_value", "poolable", "converged")]
#' # A+B passes; A+C, B+C, and A+B+C fail under both models.
#' # Keep C separate when considering A and B for pooling.
#' @export
poolability_combinations <- function(
  datasets,
  models = c("exponential", "beta_poisson"),
  alpha = 0.05,
  max_datasets = 6L
) {
  standardized <- standardize_datasets(datasets)
  n <- length(standardized)
  if (!is.numeric(max_datasets) || length(max_datasets) != 1L ||
      !is.finite(max_datasets) || max_datasets < 2 || max_datasets != floor(max_datasets)) {
    stop("`max_datasets` must be a whole number of at least two.", call. = FALSE)
  }
  if (n < 2L || n > max_datasets) {
    stop(sprintf("Combination reporting requires between two and %d datasets (got %d).", max_datasets, n), call. = FALSE)
  }
  models <- validate_model_set(models)
  validate_alpha(alpha)
  # ponytail: refit subsets up to the collection limit; cache fits if larger collections are needed.
  subsets <- unlist(lapply(2:n, function(size) utils::combn(n, size, simplify = FALSE)), recursive = FALSE)
  purrr::map_dfr(subsets, function(idx) {
    labels <- names(standardized)[idx]
    result <- poolability_test(standardized[idx], models = models, alpha = alpha)
    dplyr::bind_cols(
      tibble::tibble(combination = rep(paste(labels, collapse = " + "), nrow(result)),
                     datasets = rep(list(labels), nrow(result))),
      result
    )
  })
}

#' Group dose-response datasets into mutually poolable sets
#'
#' Assigns datasets to groups such that the datasets within a group are
#' statistically poolable (via the [poolability_test()] likelihood-ratio test)
#' with one selected non-overlapping grouping. This does not imply that every
#' cross-group subset fails the pooling test. Grouping is performed per model,
#' because poolability can differ between the exponential and beta-Poisson
#' models.
#'
#' @inheritParams poolability_test
#' @param method Grouping strategy. `"agglomerative"` (default) starts from
#'   singletons and repeatedly merges the most compatible poolable pair until no
#'   pair is poolable; it scales to many datasets. `"exhaustive"` enumerates all
#'   set partitions and picks the coarsest one whose every group is poolable; it
#'   is limited to `max_exhaustive` datasets.
#' @param max_exhaustive Maximum number of datasets allowed for the exhaustive
#'   method (the number of set partitions grows super-exponentially).
#'
#' @return A long tibble with columns `dataset`, `model`, and integer `group`.
#'   The per-model matrix of pairwise pooling p-values is attached as the
#'   `"pairwise"` attribute.
#' @details Group numbers are labels within a model, not rankings. This function
#' returns assignments, not pooled datasets or fitted models. Use
#' [poolability_combinations()] to inspect overlapping candidate combinations.
#' Check metadata compatibility before grouping; it is not enforced here.
#' @examples
#' path <- system.file("extdata", "pooling-example.csv", package = "singlehit")
#' raw <- readr::read_csv(path, show_col_types = FALSE)
#' trials <- split(raw, raw$study_id)
#' groups <- group_datasets(trials, method = "exhaustive")
#' groups
#' # For both models, A and B share group 1; C is in group 2.
#' attr(groups, "pairwise")
#' @export
group_datasets <- function(
  datasets,
  models = c("exponential", "beta_poisson"),
  alpha = 0.05,
  method = c("agglomerative", "exhaustive"),
  max_exhaustive = 6L
) {
  standardized <- standardize_datasets(datasets)
  models <- validate_model_set(models)
  validate_alpha(alpha)
  method <- match.arg(method)
  m <- length(standardized)
  dataset_labels <- names(standardized)

  if (m == 1L) {
    return(purrr::map_dfr(models, function(model) {
      tibble::tibble(dataset = dataset_labels, model = model, group = 1L)
    }))
  }
  if (method == "exhaustive" && m > max_exhaustive) {
    stop(
      sprintf(
        "Exhaustive grouping is limited to %d datasets (got %d); use method = \"agglomerative\".",
        max_exhaustive,
        m
      ),
      call. = FALSE
    )
  }

  pairwise <- list()
  result <- purrr::map_dfr(models, function(model) {
    deviance_individual <- vapply(standardized, function(dataset) fit_core(dataset, model)$deviance, numeric(1))
    pairwise[[model]] <<- pairwise_pooling_matrix(standardized, deviance_individual, model)
    groups <- if (method == "agglomerative") {
      agglomerative_groups(standardized, deviance_individual, model, alpha)
    } else {
      exhaustive_groups(standardized, deviance_individual, model, alpha)
    }
    tibble::tibble(dataset = dataset_labels, model = model, group = groups)
  })
  attr(result, "pairwise") <- pairwise
  result
}

# ---- internal helpers -------------------------------------------------------

standardize_datasets <- function(datasets) {
  if (!is.list(datasets) || is.data.frame(datasets) || length(datasets) == 0L) {
    stop("`datasets` must be a non-empty list of dose-response datasets.", call. = FALSE)
  }
  names(datasets) <- dataset_names(datasets)
  purrr::imap(datasets, function(dataset, name) {
    tryCatch(
      as_dose_response(dataset),
      error = function(cnd) {
        stop(sprintf("Dataset '%s': %s", name, conditionMessage(cnd)), call. = FALSE)
      }
    )
  })
}

dataset_names <- function(datasets) {
  labels <- names(datasets)
  if (is.null(labels)) {
    labels <- rep("", length(datasets))
  }
  blank <- !nzchar(labels)
  labels[blank] <- paste0("dataset_", seq_along(datasets))[blank]
  make.unique(labels)
}

validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number between zero and one.", call. = FALSE)
  }
  invisible(alpha)
}

pool_conclusion <- function(model, poolable) {
  label <- model_label(model)
  if (poolable) {
    paste0("The datasets are poolable under the ", label, " model.")
  } else {
    paste0("The datasets are not poolable under the ", label, " model.")
  }
}

# p-value for pooling the datasets at positions `idx`. A singleton is trivially
# poolable (no test), returning 1.
pool_p_value <- function(standardized, deviance_individual, model, idx) {
  size <- length(idx)
  if (size < 2L) {
    return(1)
  }
  k <- length(model_parameters(model))
  pooled_deviance <- fit_core(dplyr::bind_rows(standardized[idx]), model)$deviance
  lrt <- max(pooled_deviance - sum(deviance_individual[idx]), 0)
  stats::pchisq(lrt, df = k * (size - 1L), lower.tail = FALSE)
}

pairwise_pooling_matrix <- function(standardized, deviance_individual, model) {
  m <- length(standardized)
  labels <- names(standardized)
  matrix_out <- matrix(NA_real_, m, m, dimnames = list(labels, labels))
  diag(matrix_out) <- 1
  for (i in seq_len(m - 1L)) {
    for (j in (i + 1L):m) {
      p <- pool_p_value(standardized, deviance_individual, model, c(i, j))
      matrix_out[i, j] <- p
      matrix_out[j, i] <- p
    }
  }
  matrix_out
}

# Greedy agglomerative grouping: merge the most compatible poolable pair of
# groups until none remain poolable. Individual deviances are cached; only the
# merged-group deviance is refit each step.
agglomerative_groups <- function(standardized, deviance_individual, model, alpha) {
  groups <- as.list(seq_along(standardized))
  repeat {
    n <- length(groups)
    if (n < 2L) {
      break
    }
    best_p <- alpha
    best_pair <- NULL
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        p <- pool_p_value(standardized, deviance_individual, model, c(groups[[i]], groups[[j]]))
        if (p > best_p) {
          best_p <- p
          best_pair <- c(i, j)
        }
      }
    }
    if (is.null(best_pair)) {
      break
    }
    merged <- c(groups[[best_pair[1L]]], groups[[best_pair[2L]]])
    groups <- c(groups[-best_pair], list(merged))
  }
  group_assignment(groups, length(standardized))
}

# Exhaustive grouping: keep every set partition whose groups are all poolable,
# then choose the coarsest (fewest groups), tie-broken by the largest minimum
# within-group pooling p-value.
exhaustive_groups <- function(standardized, deviance_individual, model, alpha) {
  partitions <- set_partitions(length(standardized))
  valid <- Filter(
    function(partition) {
      all(vapply(
        partition,
        function(idx) pool_p_value(standardized, deviance_individual, model, idx) > alpha,
        logical(1)
      ))
    },
    partitions
  )
  # The all-singletons partition is always valid (each block is trivially
  # poolable), so `valid` is never empty.
  sizes <- lengths(valid)
  coarsest <- valid[sizes == min(sizes)]
  min_p <- vapply(
    coarsest,
    function(partition) {
      min(vapply(
        partition,
        function(idx) pool_p_value(standardized, deviance_individual, model, idx),
        numeric(1)
      ))
    },
    numeric(1)
  )
  group_assignment(coarsest[[which.max(min_p)]], length(standardized))
}

# Turn a list of index vectors into a deterministic integer group id per dataset
# (groups labelled 1.. in order of their smallest member).
group_assignment <- function(groups, m) {
  groups <- groups[order(vapply(groups, min, integer(1)))]
  assignment <- integer(m)
  for (g in seq_along(groups)) {
    assignment[groups[[g]]] <- g
  }
  assignment
}

# All set partitions of {1..n} as a list of partitions, each a list of index
# vectors. Grows as the Bell number, so callers cap n (see `max_exhaustive`).
set_partitions <- function(n) {
  if (n == 0L) {
    return(list(list()))
  }
  previous <- set_partitions(n - 1L)
  result <- list()
  for (partition in previous) {
    for (block in seq_along(partition)) {
      extended <- partition
      extended[[block]] <- c(extended[[block]], n)
      result[[length(result) + 1L]] <- extended
    }
    result[[length(result) + 1L]] <- c(partition, list(n))
  }
  result
}
