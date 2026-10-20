#' mirai compute profile shared by every bootstrap run in the app
#'
#' One named pool serves all sessions and all models. Without it each async
#' bootstrap job would start its own pool sized to 75% of the cores, and only
#' `collect_bootstrap()` shuts a job's own pool down.
#' @noRd
bootstrap_compute_profile <- "singlehitapp"

#' Default number of bootstrap workers
#'
#' 75% of the physical cores, capped at 8 so a hosted deployment on a many-core
#' machine does not claim all of its memory. Override with
#' `options(singlehitapp.workers = n)`.
#'
#' @param cores Detected core count; injectable for tests.
#' @return A whole number of at least 1.
#' @noRd
default_bootstrap_workers <- function(cores = parallel::detectCores(logical = FALSE)) {
  chosen <- getOption("singlehitapp.workers")
  if (!is.null(chosen)) {
    return(max(1L, as.integer(chosen)))
  }
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    cores <- parallel::detectCores(logical = TRUE)
  }
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    return(1L)
  }
  max(1L, min(8L, as.integer(floor(0.75 * cores))))
}

#' Start the shared bootstrap workers if they are not running
#'
#' Safe to call before every run. Starting the pool ourselves means no job
#' "owns" it, so one session finishing a job cannot shut the pool down under
#' another session's job.
#'
#' @param workers Number of daemons to start.
#' @noRd
start_bootstrap_workers <- function(workers = default_bootstrap_workers()) {
  if (!mirai::daemons_set(.compute = bootstrap_compute_profile)) {
    mirai::daemons(workers, .compute = bootstrap_compute_profile)
  }
  invisible(TRUE)
}

#' Stop the shared bootstrap workers if they are running
#' @noRd
stop_bootstrap_workers <- function() {
  if (mirai::daemons_set(.compute = bootstrap_compute_profile)) {
    mirai::daemons(0L, .compute = bootstrap_compute_profile)
  }
  invisible(TRUE)
}
