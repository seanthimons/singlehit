# Benchmark how long a bootstrap run blocks the app and how long it takes in total.
#
# "Blocked" is the time the calling R process is busy starting the run, which is
# when a Shiny app cannot respond to anyone. "Total" is until every batch is
# collected. Run from the app/ directory:
#
#   source("dev/benchmark_bootstrap.R")
#   results <- benchmark_bootstrap()                      # cheap models
#   results <- benchmark_bootstrap(exact_times = c(200))  # add the exact model
#
# It loads the in-repo singlehit, so it measures the code in your checkout.
pkgload::load_all("..", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)

benchmark_bootstrap <- function(
  times = c(1000L, 2500L, 5000L, 10000L),
  reps = 3L,
  workers = 8L,
  exact_times = integer(),
  models = c("exponential", "beta_poisson")
) {
  data <- singlehit::read_dose_response(system.file("extdata", "Ward_rotavirus.txt", package = "singlehit"))
  all_models <- union(models, if (length(exact_times) > 0L) "exact_beta_poisson")
  analysis <- singlehit::analyze_dose_response(
    data,
    models = all_models,
    bootstrap_times = 0L,
    exact_bootstrap_times = 0L,
    check_trend = FALSE
  )
  profile <- "benchmark"
  mirai::daemons(workers, .compute = profile)
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)

  run_once <- function(fits, n) {
    started <- Sys.time()
    jobs <- lapply(fits, function(fit) {
      singlehit::bootstrap_dose_response_async(fit, times = n, seed = 1, compute = profile)
    })
    blocked <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    while (!all(vapply(jobs, function(job) singlehit::bootstrap_progress(job)$finished, logical(1)))) {
      Sys.sleep(0.2)
    }
    lapply(jobs, singlehit::collect_bootstrap)
    c(blocked = blocked, total = as.numeric(difftime(Sys.time(), started, units = "secs")))
  }

  cases <- rbind(
    data.frame(models = paste(models, collapse = " + "), replicates = times, exact = FALSE),
    if (length(exact_times) > 0L) data.frame(models = "exact_beta_poisson", replicates = exact_times, exact = TRUE)
  )
  rows <- lapply(seq_len(nrow(cases)), function(i) {
    fits <- if (cases$exact[[i]]) analysis$fits["exact_beta_poisson"] else analysis$fits[models]
    n_reps <- if (cases$exact[[i]]) 1L else reps
    runs <- t(vapply(seq_len(n_reps), function(r) run_once(fits, cases$replicates[[i]]), numeric(2)))
    cat(sprintf(
      "%-28s n=%5d: blocked %5.2fs (max %5.2fs) | total %6.1fs (%d run%s)\n",
      cases$models[[i]],
      cases$replicates[[i]],
      mean(runs[, "blocked"]),
      max(runs[, "blocked"]),
      mean(runs[, "total"]),
      n_reps,
      if (n_reps == 1L) "" else "s"
    ))
    data.frame(
      cases[i, c("models", "replicates")],
      workers = workers,
      blocked_mean = mean(runs[, "blocked"]),
      blocked_max = max(runs[, "blocked"]),
      total_mean = mean(runs[, "total"]),
      runs = n_reps
    )
  })
  invisible(do.call(rbind, rows))
}
