test_that("the default worker count is 75% of the cores, capped at 8, and at least 1", {
  withr::local_options(singlehitapp.workers = NULL)
  expect_equal(default_bootstrap_workers(cores = 16L), 8L)
  expect_equal(default_bootstrap_workers(cores = 8L), 6L)
  expect_equal(default_bootstrap_workers(cores = 4L), 3L)
  expect_equal(default_bootstrap_workers(cores = 1L), 1L)
})

test_that("an unknown core count falls back to a usable number", {
  withr::local_options(singlehitapp.workers = NULL)
  workers <- default_bootstrap_workers(cores = NA_integer_)
  expect_true(is.integer(workers) && workers >= 1L && workers <= 8L)
})

test_that("the singlehitapp.workers option overrides the default", {
  withr::local_options(singlehitapp.workers = 2)
  expect_equal(default_bootstrap_workers(cores = 16L), 2L)
  withr::local_options(singlehitapp.workers = 0)
  expect_equal(default_bootstrap_workers(cores = 16L), 1L)
})

test_that("starting and stopping the shared workers is idempotent", {
  skip_if_not_installed("mirai")
  withr::defer(stop_bootstrap_workers())
  expect_false(mirai::daemons_set(.compute = bootstrap_compute_profile))

  start_bootstrap_workers(2)
  expect_true(mirai::daemons_set(.compute = bootstrap_compute_profile))
  expect_no_error(start_bootstrap_workers(2))
  expect_true(mirai::daemons_set(.compute = bootstrap_compute_profile))

  stop_bootstrap_workers()
  expect_false(mirai::daemons_set(.compute = bootstrap_compute_profile))
  expect_no_error(stop_bootstrap_workers())
})
