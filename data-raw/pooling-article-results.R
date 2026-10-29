# Precompute the slow results in the pooling article
# (vignettes/articles/pooling.Rmd): the two group_datasets() searches take
# about a minute together, so the article reads them from
# vignettes/articles/pooling-results.rds instead of rerunning them on every
# render (see #39). Rerun this after changing the pooling code or the example
# data. The article stops with a message if the data no longer match, or if a
# live test of one pair disagrees with the saved pairwise p-values.
#
# Run from the package root:
# Rscript data-raw/pooling-article-results.R
pkgload::load_all(".", quiet = TRUE)

path <- "inst/extdata/cryptosporidium-isolates.csv"
raw <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(study_id = "c"))
trials <- split(raw, raw$study_id)

warnings <- character()
explore <- withCallingHandlers(
  group_datasets(trials, method = "agglomerative"),
  warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
exhaustive <- suppressWarnings(group_datasets(trials, method = "exhaustive"))

saveRDS(
  list(
    explore = explore,
    exhaustive = exhaustive,
    warnings = unique(warnings),
    # The data themselves, not a file checksum, so line-ending conversion on
    # checkout does not look like a change.
    data = as.data.frame(raw),
    singlehit_version = as.character(utils::packageVersion("singlehit")),
    created = format(Sys.Date())
  ),
  "vignettes/articles/pooling-results.rds"
)
cat("Wrote vignettes/articles/pooling-results.rds\n")
