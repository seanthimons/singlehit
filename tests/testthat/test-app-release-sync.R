# The app version sync lives with the CI scripts, which are not part of the
# built package, so these tests only run from a source checkout.
script <- test_path("..", "..", ".github", "scripts", "sync-app-release.R")
skip_if_not(file.exists(script), "release script is not in the built package")
skip_if_not_installed("desc")
sync_env <- new.env()
sys.source(script, envir = sync_env)

app_copy <- function() {
  app <- tempfile("app")
  dir.create(file.path(app, "inst"), recursive = TRUE)
  writeLines(
    c(
      "Package: singlehitapp",
      "Version: 0.0.0.9000",
      "Imports:",
      "    shiny,",
      "    singlehit (>= 0.3.0)",
      "Remotes:",
      "    seanthimons/singlehit"
    ),
    file.path(app, "DESCRIPTION")
  )
  writeLines(
    c("default:", "  golem_name: singlehitapp", "  golem_version: 0.0.0.9000"),
    file.path(app, "inst", "golem-config.yml")
  )
  app
}

test_that("syncing sets the app's version, singlehit floor, remote tag, and golem version", {
  app <- app_copy()
  expect_true(sync_env$sync_app_release("0.4.0", app))
  d <- read.dcf(file.path(app, "DESCRIPTION"))
  expect_equal(d[, "Version"], c(Version = "0.4.0"))
  expect_match(d[, "Imports"], "singlehit (>= 0.4.0)", fixed = TRUE)
  expect_equal(trimws(d[, "Remotes"]), c(Remotes = "seanthimons/singlehit@v0.4.0"))
  expect_true("  golem_version: 0.4.0" %in% readLines(file.path(app, "inst", "golem-config.yml")))
  expect_false(any(grepl("[ \t]$", readLines(file.path(app, "DESCRIPTION")))))
  expect_false(sync_env$sync_app_release("0.4.0", app))
})

test_that("check-only mode passes once synced and fails otherwise", {
  app <- app_copy()
  expect_error(sync_env$sync_app_release("0.4.0", app, check_only = TRUE), "not released in step")
  sync_env$sync_app_release("0.4.0", app)
  expect_silent(sync_env$sync_app_release("0.4.0", app, check_only = TRUE))
})
